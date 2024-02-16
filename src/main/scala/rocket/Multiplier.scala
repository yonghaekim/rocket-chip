// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util.{Cat, log2Up, log2Ceil, log2Floor, Log2, Decoupled, Enum, Fill, Valid, Pipe}
import Chisel.ImplicitConversions._
import freechips.rocketchip.util._
import ALU._

class MultiplierReq(dataBits: Int, tagBits: Int) extends Bundle {
  val fn = Bits(SZ_ALU_FN.W)
  val dw = Bits(SZ_DW.W)
  val in1 = Bits(dataBits.W)
  val in2 = Bits(dataBits.W)
  val tag = UInt(tagBits.W)
}

class MultiplierResp(dataBits: Int, tagBits: Int) extends Bundle {
  val data = Bits(dataBits.W)
  val tag = UInt(tagBits.W)
}

class MultiplierIO(val dataBits: Int, val tagBits: Int) extends Bundle {
  val req = Flipped(Decoupled(new MultiplierReq(dataBits, tagBits)))
  val kill = Input(Bool())
  val resp = Decoupled(new MultiplierResp(dataBits, tagBits))
}

case class MulDivParams(
  mulUnroll: Int = 1,
  divUnroll: Int = 1,
  mulEarlyOut: Boolean = false,
  divEarlyOut: Boolean = false,
  divEarlyOutGranularity: Int = 1
)

class MulDiv(cfg: MulDivParams, width: Int, nXpr: Int = 32) extends Module {
  private def minDivLatency = (cfg.divUnroll > 0).option(if (cfg.divEarlyOut) 3 else 1 + w/cfg.divUnroll)
  private def minMulLatency = (cfg.mulUnroll > 0).option(if (cfg.mulEarlyOut) 2 else w/cfg.mulUnroll)
  def minLatency: Int = (minDivLatency ++ minMulLatency).min

  val io = IO(new MultiplierIO(width, log2Up(nXpr)))
  val w = io.req.bits.in1.getWidth
  val mulw = if (cfg.mulUnroll == 0) w else (w + cfg.mulUnroll - 1) / cfg.mulUnroll * cfg.mulUnroll
  val fastMulW = if (cfg.mulUnroll == 0) false else w/2 > cfg.mulUnroll && w % (2*cfg.mulUnroll) == 0
 
  val s_ready :: s_neg_inputs :: s_mul :: s_div :: s_dummy :: s_neg_output :: s_done_mul :: s_done_div :: Nil = Enum(8)
  val state = RegInit(s_ready)
 
  val req = Reg(chiselTypeOf(io.req.bits))
  val count = Reg(UInt(log2Ceil(
    ((cfg.divUnroll != 0).option(w/cfg.divUnroll + 1).toSeq ++
     (cfg.mulUnroll != 0).option(mulw/cfg.mulUnroll)).reduce(_ max _)).W))
  val neg_out = Reg(Bool())
  val isHi = Reg(Bool())
  val resHi = Reg(Bool())
  val divisor = Reg(Bits((w+1).W)) // div only needs w bits
  val remainder = Reg(Bits((2*mulw+2).W)) // div only needs 2*w+1 bits

  val mulDecode = List(
    FN_MUL    -> List(Y, N, X, X),
    FN_MULH   -> List(Y, Y, Y, Y),
    FN_MULHU  -> List(Y, Y, N, N),
    FN_MULHSU -> List(Y, Y, Y, N))
  val divDecode = List(
    FN_DIV    -> List(N, N, Y, Y),
    FN_REM    -> List(N, Y, Y, Y),
    FN_DIVU   -> List(N, N, N, N),
    FN_REMU   -> List(N, Y, N, N))
  val cmdMul :: cmdHi :: lhsSigned :: rhsSigned :: Nil =
    DecodeLogic(io.req.bits.fn, List(X, X, X, X),
      (if (cfg.divUnroll != 0) divDecode else Nil) ++ (if (cfg.mulUnroll != 0) mulDecode else Nil)).map(_.asBool)

  require(w == 32 || w == 64)
  def halfWidth(req: MultiplierReq) = (w > 32).B && req.dw === DW_32

  def sext(x: Bits, halfW: Bool, signed: Bool) = {
    val sign = signed && Mux(halfW, x(w/2-1), x(w-1))
    val hi = Mux(halfW, Fill(w/2, sign), x(w-1,w/2))
    (Cat(hi, x(w/2-1,0)), sign)
  }
  val (lhs_in, lhs_sign) = sext(io.req.bits.in1, halfWidth(io.req.bits), lhsSigned)
  val (rhs_in, rhs_sign) = sext(io.req.bits.in2, halfWidth(io.req.bits), rhsSigned)
  
  val subtractor = remainder(2*w,w) - divisor
  val result = Mux(resHi, remainder(2*w, w+1), remainder(w-1, 0))
  val negated_remainder = -result

  if (cfg.divUnroll != 0) when (state === s_neg_inputs) {
    when (remainder(w-1)) {
      remainder := negated_remainder
    }
    when (divisor(w-1)) {
      divisor := subtractor
    }
    state := s_div
  }
  if (cfg.divUnroll != 0) when (state === s_neg_output) {
    remainder := negated_remainder
    state := s_done_div
    resHi := false
  }
  if (cfg.mulUnroll != 0) when (state === s_mul) {
    val mulReg = Cat(remainder(2*mulw+1,w+1),remainder(w-1,0))
    val mplierSign = remainder(w)
    val mplier = mulReg(mulw-1,0)
    val accum = mulReg(2*mulw,mulw).asSInt
    val mpcand = divisor.asSInt
    val prod = Cat(mplierSign, mplier(cfg.mulUnroll-1, 0)).asSInt * mpcand + accum
    val nextMulReg = Cat(prod, mplier(mulw-1, cfg.mulUnroll))
    val nextMplierSign = count === mulw/cfg.mulUnroll-2 && neg_out

    val eOutMask = ((BigInt(-1) << mulw).S >> (count * cfg.mulUnroll)(log2Up(mulw)-1,0))(mulw-1,0)
    val eOut = (cfg.mulEarlyOut).B && count =/= mulw/cfg.mulUnroll-1 && count =/= 0 &&
      !isHi && (mplier & ~eOutMask) === 0.U
    val eOutRes = (mulReg >> (mulw - count * cfg.mulUnroll)(log2Up(mulw)-1,0))
    val nextMulReg1 = Cat(nextMulReg(2*mulw,mulw), Mux(eOut, eOutRes, nextMulReg)(mulw-1,0))
    remainder := Cat(nextMulReg1 >> w, nextMplierSign, nextMulReg1(w-1,0))

    count := count + 1
    when (eOut || count === mulw/cfg.mulUnroll-1) {
      state := s_done_mul
      resHi := isHi
    }
  }
  if (cfg.divUnroll != 0) when (state === s_div) {
    val unrolls = ((0 until cfg.divUnroll) scanLeft remainder) { case (rem, i) =>
      // the special case for iteration 0 is to save HW, not for correctness
      val difference = if (i == 0) subtractor else rem(2*w,w) - divisor(w-1,0)
      val less = difference(w)
      Cat(Mux(less, rem(2*w-1,w), difference(w-1,0)), rem(w-1,0), !less)
    } tail

    remainder := unrolls.last
    when (count === w/cfg.divUnroll) {
      state := Mux(neg_out, s_neg_output, s_done_div)
      resHi := isHi
      if (w % cfg.divUnroll < cfg.divUnroll - 1)
        remainder := unrolls(w % cfg.divUnroll)
    }
    count := count + 1

    val divby0 = count === 0 && !subtractor(w)
    if (cfg.divEarlyOut) {
      val align = 1 << log2Floor(cfg.divUnroll max cfg.divEarlyOutGranularity)
      val alignMask = ~((align-1).U(log2Ceil(w).W))
      val divisorMSB = Log2(divisor(w-1,0), w) & alignMask
      val dividendMSB = Log2(remainder(w-1,0), w) | ~alignMask
      val eOutPos = ~(dividendMSB - divisorMSB)
      val eOut = count === 0 && !divby0 && eOutPos >= align
      when (eOut) {
        remainder := remainder(w-1,0) << eOutPos
        count := eOutPos >> log2Floor(cfg.divUnroll)
      }
    }
    when (divby0 && !isHi) { neg_out := false }
  }
  when (io.resp.fire() || io.kill) {
    state := s_ready
  }
  when (io.req.fire()) {
    state := Mux(cmdMul, s_mul, Mux(lhs_sign || rhs_sign, s_neg_inputs, s_div))
    isHi := cmdHi
    resHi := false
    count := (if (fastMulW) Mux[UInt](cmdMul && halfWidth(io.req.bits), w/cfg.mulUnroll/2, 0) else 0)
    neg_out := Mux(cmdHi, lhs_sign, lhs_sign =/= rhs_sign)
    divisor := Cat(rhs_sign, rhs_in)
    remainder := lhs_in
    req := io.req.bits
  }

  val outMul = (state & (s_done_mul ^ s_done_div)) === (s_done_mul & ~s_done_div)
  val loOut = Mux(fastMulW.B && halfWidth(req) && outMul, result(w-1,w/2), result(w/2-1,0))
  val hiOut = Mux(halfWidth(req), Fill(w/2, loOut(w/2-1)), result(w-1,w/2))
  io.resp.bits.tag := req.tag

  io.resp.bits.data := Cat(hiOut, loOut)
  io.resp.valid := (state === s_done_mul || state === s_done_div)
  io.req.ready := state === s_ready
}

class PipelinedMultiplier(width: Int, latency: Int, nXpr: Int = 32) extends Module with ShouldBeRetimed {
  val io = IO(new Bundle {
    val req = Flipped(Valid(new MultiplierReq(width, log2Ceil(nXpr))))
    val resp = Valid(new MultiplierResp(width, log2Ceil(nXpr)))
  })

  val in = Pipe(io.req)

  val decode = List(
    FN_MUL    -> List(N, X, X),
    FN_MULH   -> List(Y, Y, Y),
    FN_MULHU  -> List(Y, N, N),
    FN_MULHSU -> List(Y, Y, N))
  val cmdHi :: lhsSigned :: rhsSigned :: Nil =
    DecodeLogic(in.bits.fn, List(X, X, X), decode).map(_.asBool)
  val cmdHalf = (width > 32).B && in.bits.dw === DW_32

  val lhs = Cat(lhsSigned && in.bits.in1(width-1), in.bits.in1).asSInt
  val rhs = Cat(rhsSigned && in.bits.in2(width-1), in.bits.in2).asSInt
  val prod = lhs * rhs
  val muxed = Mux(cmdHi, prod(2*width-1, width), Mux(cmdHalf, prod(width/2-1, 0).sextTo(width), prod(width-1, 0)))

  val resp = Pipe(in, latency-1)
  io.resp.valid := resp.valid
  io.resp.bits.tag := resp.bits.tag
  //yh-io.resp.bits.data := Pipe(in.valid, muxed, latency-1).bits

	//yh+begin
  // This is Chisel implementation of QARMA
  // written based on the C implementation of QARMA64:
  // https://github.com/Phantom1003/QARMA64
  // QARMA paper: https://eprint.iacr.org/2016/444.pdf
	val w0 = WireInit(0x84be85ce9804e94bL.S(64.W))
	val k0 = WireInit(0xec2802d4e0a488e9L.S(64.W))
	val w1 = WireInit(0xc25f42e74c0274a4L.S(64.W))
	val k1 = WireInit(0xec2802d4e0a488e9L.S(64.W))
	val check_box = Wire(Vec(3, SInt(64.W)))
	check_box(0) := 0xc003b93999b33765L.S
	check_box(1) := 0x270a787275c48d10L.S
	check_box(2) := 0x5c06a7501b63b2fdL.S

	val alpha = WireInit(0xC0AC29B7C97C50DDL.S(64.W))
	val c = Wire(Vec(8, SInt(64.W)))
	c(0) := 0x0000000000000000L.S
	c(1) := 0x13198A2E03707344L.S
	c(2) := 0xA4093822299F31D0L.S
	c(3) := 0x082EFA98EC4E6C89L.S
	c(4) := 0x452821E638D01377L.S
	c(5) := 0xBE5466CF34E90C6CL.S
	c(6) := 0x3F84D5B5B5470917L.S
	c(7) := 0x9216D5D98979FB1BL.S

	val sbox = Wire(Vec(16, UInt(8.W)))
	sbox( 0) := 11.U
	sbox( 1) := 6.U
	sbox( 2) := 8.U
	sbox( 3) := 15.U
	sbox( 4) := 12.U
	sbox( 5) := 0.U
	sbox( 6) := 9.U
	sbox( 7) := 14.U
	sbox( 8) := 3.U
	sbox( 9) := 7.U
	sbox(10) := 4.U
	sbox(11) := 5.U
	sbox(12) := 13.U
	sbox(13) := 2.U
	sbox(14) := 1.U
	sbox(15) := 10.U

	val sbox_inv = Wire(Vec(16, UInt(8.W)))
	sbox_inv( 0) := 5.U
	sbox_inv( 1) := 14.U
	sbox_inv( 2) := 13.U
	sbox_inv( 3) := 8.U
	sbox_inv( 4) := 10.U
	sbox_inv( 5) := 11.U
	sbox_inv( 6) := 1.U
	sbox_inv( 7) := 9.U
	sbox_inv( 8) := 2.U
	sbox_inv( 9) := 6.U
	sbox_inv(10) := 15.U
	sbox_inv(11) := 0.U
	sbox_inv(12) := 4.U
	sbox_inv(13) := 12.U
	sbox_inv(14) := 7.U
	sbox_inv(15) := 3.U

	val t = Wire(Vec(16, UInt(8.W)))
	t( 0) := 0.U
	t( 1) := 11.U
	t( 2) := 6.U
	t( 3) := 13.U
	t( 4) := 10.U
	t( 5) := 1.U
	t( 6) := 12.U
	t( 7) := 7.U
	t( 8) := 5.U
	t( 9) := 14.U
	t(10) := 3.U
	t(11) := 8.U
	t(12) := 15.U
	t(13) := 4.U
	t(14) := 9.U
	t(15) := 2.U

	val t_inv = Wire(Vec(16, UInt(8.W)))
	t_inv( 0) := 0.U
	t_inv( 1) := 5.U
	t_inv( 2) := 15.U
	t_inv( 3) := 10.U
	t_inv( 4) := 13.U
	t_inv( 5) := 8.U
	t_inv( 6) := 2.U
	t_inv( 7) := 7.U
	t_inv( 8) := 11.U
	t_inv( 9) := 14.U
	t_inv(10) := 4.U
	t_inv(11) := 1.U
	t_inv(12) := 6.U
	t_inv(13) := 3.U
	t_inv(14) := 9.U
	t_inv(15) := 12.U

	val h = Wire(Vec(16, UInt(8.W)))
	h( 0) := 6.U
	h( 1) := 5.U
	h( 2) := 14.U
	h( 3) := 15.U
	h( 4) := 0.U
	h( 5) := 1.U
	h( 6) := 2.U
	h( 7) := 3.U
	h( 8) := 7.U
	h( 9) := 12.U
	h(10) := 13.U
	h(11) := 4.U
	h(12) := 8.U
	h(13) := 9.U
	h(14) := 10.U
	h(15) := 11.U

	val h_inv = Wire(Vec(16, UInt(8.W)))
	h_inv( 0) := 4.U
	h_inv( 1) := 5.U
	h_inv( 2) := 6.U
	h_inv( 3) := 7.U
	h_inv( 4) := 11.U
	h_inv( 5) := 1.U
	h_inv( 6) := 0.U
	h_inv( 7) := 8.U
	h_inv( 8) := 12.U
	h_inv( 9) := 13.U
	h_inv(10) := 14.U
	h_inv(11) := 15.U
	h_inv(12) := 9.U
	h_inv(13) := 10.U
	h_inv(14) := 2.U
	h_inv(15) := 3.U

	val M = Wire(Vec(16, UInt(8.W)))
	M( 0) := 0.U
	M( 1) := 1.U
	M( 2) := 2.U
	M( 3) := 1.U
	M( 4) := 1.U
	M( 5) := 0.U
	M( 6) := 1.U
	M( 7) := 2.U
	M( 8) := 2.U
	M( 9) := 1.U
	M(10) := 0.U
	M(11) := 1.U
	M(12) := 1.U
	M(13) := 2.U
	M(14) := 1.U
	M(15) := 0.U

	def cell2text(cell: chisel3.Vec[chisel3.UInt]): UInt = {
		val bytes = Wire(Vec(8, UInt(8.W)))
		for (i <- 0 until 8) {
			bytes(i) := (cell(2*i) << 4) | cell(2*i+1)
		}

		val res = Cat(bytes(0), bytes(1), bytes(2), bytes(3),
									bytes(4), bytes(5), bytes(6), bytes(7))
		res
	}

	def forward(is: UInt, tk: UInt, r: Int): UInt = {
		val cell = Wire(Vec(16, UInt(8.W)))
		val cell2 = Wire(Vec(16, UInt(8.W)))
		val cell3 = Wire(Vec(16, UInt(8.W)))

		for (i <- 0 until 16) {
			cell(i) := 0.U
			cell2(i) := 0.U
			cell3(i) := 0.U
		}

		// test2cell
		for (i <- 0 until 8) {
			cell(2*(7-i)+0) := (((is ^ tk)(i*8+7, i*8) & 0xF0.U) >> 4)
			cell(2*(7-i)+1) := ((is ^ tk)(i*8+7, i*8) & 0xF.U)
		}


		if (r != 0) {
			// ShuffleCells
			val perm = Wire(Vec(16, UInt(8.W)))

			for (i <- 0 until 16) {
				perm(i) := cell(t(i))
			}

			// MixColumns
			for (x <- 0 until 4) {
				for (y <- 0 until 4) {
					var temp = 0.U

					for (j <- 0 until 4) {
						val b = M(4*x+j)
						val a = perm(4*j+y)
						temp = Mux(b =/= 0.U, 
										temp ^ (((a << b) & 0x0F.U) | (a >> (4.U - b))), temp)
					}
					cell2(4*x+y) := temp
				}
			}
		}

		if (r == 0) {
			// SubCells
			for (i <- 0 until 16) {
				cell3(i) := sbox(cell(i))
			}
		} else {
			// SubCells
			for (i <- 0 until 16) {
				cell3(i) := sbox(cell2(i))
			}
		}

		// cell2text
		cell2text(cell3)
	}

	def LFSR(x: UInt): UInt = {
		val b0 = ((x >> 0) & 1.U)
		val b1 = ((x >> 1) & 1.U)
		val b2 = ((x >> 2) & 1.U)
		val b3 = ((x >> 3) & 1.U)
		val res = (((b0 ^ b1) << 3) | (b3 << 2) | (b2 << 1) | (b1 << 0))
		res
	}

	def LFSR_inv(x: UInt): UInt = {
		val b0 = ((x >> 0) & 1.U)
		val b1 = ((x >> 1) & 1.U)
		val b2 = ((x >> 2) & 1.U)
		val b3 = ((x >> 3) & 1.U)
		val res = (((b0 ^ b3) << 0) | (b0 << 1) | (b1 << 2) | (b2 << 3))
		res
	}

	def forward_update_key(T: UInt): UInt = {
		val cell = Wire(Vec(16, UInt(8.W)))
		val temp = Wire(Vec(16, UInt(8.W)))
		val temp2 = Wire(Vec(16, UInt(8.W)))

		for (i <- 0 until 16) {
			cell(i) := 0.U
			temp(i) := 0.U
		}
		
		// test2cell
		for (i <- 0 until 8) {
			cell(2*(7-i)+0) := (T(i*8+7, i*8) & 0xF0.U) >> 4
			cell(2*(7-i)+1) := (T(i*8+7, i*8) & 0xF.U)
		}

		// h box
		for (i <- 0 until 16) {
			temp(i) := cell(h(i))
		}

		for (i <- 0 until 16) {
			temp2(i) := temp(i)
		}

		temp2( 0) := LFSR(temp( 0))
		temp2( 1) := LFSR(temp( 1))
		temp2( 3) := LFSR(temp( 3))
		temp2( 4) := LFSR(temp( 4))
		temp2( 8) := LFSR(temp( 8))
		temp2(11) := LFSR(temp(11))
		temp2(13) := LFSR(temp(13))

		// cell2text
		cell2text(temp2)
	}

	def pseudo_reflect(is: UInt, tk: UInt): UInt = {
		val cell = Wire(Vec(16, UInt(8.W)))
		val cell2 = Wire(Vec(16, UInt(8.W)))
		val cell3 = Wire(Vec(16, UInt(8.W)))

		for (i <- 0 until 16) {
			cell(i) := 0.U
		}

		// test2cell
		for (i <- 0 until 8) {
			cell(2*(7-i)+0) := (is(i*8+7, i*8) & 0xF0.U) >> 4
			cell(2*(7-i)+1) := (is(i*8+7, i*8) & 0xF.U)
		}

		// ShuffleCells
		val perm = Wire(Vec(16, UInt(8.W)))
		val perm2 = Wire(Vec(16, UInt(8.W)))
		for (i <- 0 until 16) {
			perm(i) := cell(t(i))
		}

		// MixColumns
		for (x <- 0 until 4) {
			for (y <- 0 until 4) {
				var temp = 0.U

				for (j <- 0 until 4) {
					val b = M(4*x+j)
					val a = perm(4*j+y)
					temp = Mux(b =/= 0.U, 
									temp ^ (((a << b) & 0x0F.U) | (a >> (4.U - b))), temp)
				}
				cell2(4*x+y) := temp
			}
		}

		// AddRoundTweakey
		for (i <- 0 until 16) {
			cell3(i) := (cell2(i) ^ (tk >> (4 * (15 - i))) & 0xF.U)
		}

		// ShuffleCells invert
		for (i <- 0 until 16) {
			perm2(i) := cell3(t_inv(i))
		}

		// cell2text
		cell2text(perm2)
	}

	def backward(is: UInt, tk: UInt, r: Int): UInt = {
		val cell = Wire(Vec(16, UInt(8.W)))
		val cell2 = Wire(Vec(16, UInt(8.W)))
		val cell3 = Wire(Vec(16, UInt(8.W)))

		for (i <- 0 until 16) {
			cell(i) := 0.U
		}

		// test2cell
		for (i <- 0 until 8) {
			cell(2*(7-i)+0) := (is(i*8+7, i*8) & 0xF0.U) >> 4
			cell(2*(7-i)+1) := (is(i*8+7, i*8) & 0xF.U)
		}

		// SubCells
		for (i <- 0 until 16) {
			cell2(i) := sbox_inv(cell(i))
		}

		if (r != 0) {
			val mixc = Wire(Vec(16, UInt(8.W)))

			// MixColumns
			for (x <- 0 until 4) {
				for (y <- 0 until 4) {
					var temp = 0.U

					for (j <- 0 until 4) {
						val b = M(4*x+j)
						val a = cell2(4*j+y)
						temp = Mux(b =/= 0.U, 
										temp ^ (((a << b) & 0x0F.U) | (a >> (4.U - b))), temp)
					}
					mixc(4*x+y) := temp
				}
			}

			// ShuffleCells
			for (i <- 0 until 16) {
				cell3(i) := mixc(t_inv(i))
			}
		} else {
			for (i <- 0 until 16) {
				cell3(i) := cell2(i)
			}
		}

		// cell2text
		val res = cell2text(cell3)
		(res ^ tk)
	}

	def backward_update_key(T: UInt): UInt = {
		val cell = Wire(Vec(16, UInt(8.W)))
		val cell2 = Wire(Vec(16, UInt(8.W)))
		val temp = Wire(Vec(16, UInt(8.W)))

		for (i <- 0 until 16) {
			cell(i) := 0.U
		}
		
		// test2cell
		for (i <- 0 until 8) {
			cell(2*(7-i)+0) := (T(i*8+7, i*8) & 0xF0.U) >> 4
			cell(2*(7-i)+1) := (T(i*8+7, i*8) & 0xF.U)
		}

		for (i <- 0 until 16) {
			cell2(i) := cell(i)
		}

		cell2( 0) := LFSR_inv(cell( 0))
		cell2( 1) := LFSR_inv(cell( 1))
		cell2( 3) := LFSR_inv(cell( 3))
		cell2( 4) := LFSR_inv(cell( 4))
		cell2( 8) := LFSR_inv(cell( 8))
		cell2(11) := LFSR_inv(cell(11))
		cell2(13) := LFSR_inv(cell(13))

		// h box
		for (i <- 0 until 16) {
			temp(i) := cell2(h_inv(i))
		}

		// cell2text
		cell2text(temp)
	}

	assert(latency == 3)
	val is_tagd = (io.req.valid & io.req.bits.fn === FN_TAGD)
	val is_tagd_p3 = RegNext(RegNext(RegNext(is_tagd)))

	// rounds = 5
	val is_p0 = Reg(UInt(64.W))
	val tweak_p0 = Reg(UInt(64.W))

	when (is_tagd) {
		is_p0 := io.req.bits.in1(47,0) ^ w0.asUInt // ignore upper bits of pointer address
		tweak_p0 := io.req.bits.in2

		printf("[TAGD] is_tagd! in1: %x in2: %x w0: %x is_p0: %x tweak_p0: %x\n",
						io.req.bits.in1, io.req.bits.in2, w0.asUInt, is_p0, tweak_p0)
	}

	var is_v0 = is_p0
	var tweak_v0 = tweak_p0

	for (i <- 0 until 5) {
		is_v0 = forward(is_v0, (k0.asUInt ^ tweak_v0 ^ c(i).asUInt), i)
		tweak_v0 = forward_update_key(tweak_v0)
	}

	var is_v1 = is_v0
	var tweak_v1 = tweak_v0

	is_v1 = forward(is_v1, w1.asUInt ^ tweak_v1, 1);
	is_v1 = pseudo_reflect(is_v1, k1.asUInt)
	is_v1 = backward(is_v1, w0.asUInt ^ tweak_v1, 1);

	val is_p2 = RegNext(is_v1)
	val tweak_p2 = RegNext(tweak_v1)
	var is_v2 = is_p2
	var tweak_v2 = tweak_p2

	for (i <- 0 until 5) {
		tweak_v2 = backward_update_key(tweak_v2)
		is_v2 = backward(is_v2, k0.asUInt ^ tweak_v2 ^ c(4-i).asUInt ^ alpha.asUInt, 4-i)
	}

	val pac = RegNext(is_v2 ^ w1.asUInt)

  io.resp.bits.data := Mux(is_tagd_p3,
												Cat(pac(15,0), resp.bits.in1(47,0)),
												Pipe(in.valid, muxed, latency-1).bits)

	//when (is_tagd_p3) {
	//	printf("[TAGD] io.resp.bits.data: %x pac: %x resp.bits.in1: %x resp.bits.in2: %x\n",
	//					io.resp.bits.data, pac, resp.bits.in1, resp.bits.in2)
	//}
	//yh+end
}
