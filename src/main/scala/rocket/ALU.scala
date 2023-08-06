// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.CoreModule

object ALU
{
  val SZ_ALU_FN = 4
  def FN_X    = BitPat("b????")
  def FN_ADD  = UInt(0)
  def FN_SL   = UInt(1)
  def FN_SEQ  = UInt(2)
  def FN_SNE  = UInt(3)
  def FN_XOR  = UInt(4)
  def FN_SR   = UInt(5)
  def FN_OR   = UInt(6)
  def FN_AND  = UInt(7)
  //yh+begin
  def FN_TAGD = UInt(8)
  def FN_XTAG = UInt(9)
  //yh+end
  def FN_SUB  = UInt(10)
  def FN_SRA  = UInt(11)
  def FN_SLT  = UInt(12)
  def FN_SGE  = UInt(13)
  def FN_SLTU = UInt(14)
  def FN_SGEU = UInt(15)

  def FN_DIV  = FN_XOR
  def FN_DIVU = FN_SR
  def FN_REM  = FN_OR
  def FN_REMU = FN_AND

  def FN_MUL    = FN_ADD
  def FN_MULH   = FN_SL
  def FN_MULHSU = FN_SEQ
  def FN_MULHU  = FN_SNE

  def isMulFN(fn: UInt, cmp: UInt) = fn(1,0) === cmp(1,0)
  def isSub(cmd: UInt) = cmd(3)
  def isCmp(cmd: UInt) = cmd >= FN_SLT
  def cmpUnsigned(cmd: UInt) = cmd(1)
  def cmpInverted(cmd: UInt) = cmd(0)
  def cmpEq(cmd: UInt) = !cmd(3)
	//yh+begin
	def genCrc16(hash_in: UInt): UInt =
	{
			val crc = Wire(UInt(16.W))

			crc := Cat(hash_in(11) ^ hash_in(10) ^ hash_in(7)  ^ hash_in(3)
							, hash_in(10) ^ hash_in(9)  ^ hash_in(6)  ^ hash_in(2)
							, hash_in(9)  ^ hash_in(8)  ^ hash_in(5)  ^ hash_in(1)
							, hash_in(15) ^ hash_in(8)  ^ hash_in(7)  ^ hash_in(4)  ^ hash_in(0)
							, hash_in(15) ^ hash_in(14) ^ hash_in(11) ^ hash_in(10) ^ hash_in(6)
							, hash_in(14) ^ hash_in(13) ^ hash_in(10) ^ hash_in(9)  ^ hash_in(5)
							, hash_in(15) ^ hash_in(13) ^ hash_in(12) ^ hash_in(9)  ^ hash_in(8)  ^ hash_in(4)
							, hash_in(15) ^ hash_in(14) ^ hash_in(12) ^ hash_in(11) ^ hash_in(8)  ^ hash_in(7)  ^ hash_in(3)
							, hash_in(15) ^ hash_in(14) ^ hash_in(13) ^ hash_in(11) ^ hash_in(10) ^ hash_in(7)  ^ hash_in(6)  ^ hash_in(2)
							, hash_in(14) ^ hash_in(13) ^ hash_in(12) ^ hash_in(10) ^ hash_in(9)  ^ hash_in(6)  ^ hash_in(5)  ^ hash_in(1)
							, hash_in(13) ^ hash_in(12) ^ hash_in(11) ^ hash_in(9)  ^ hash_in(8)  ^ hash_in(5)  ^ hash_in(4)  ^ hash_in(0)
							, hash_in(15) ^ hash_in(12) ^ hash_in(8)  ^ hash_in(4)
							, hash_in(15) ^ hash_in(14) ^ hash_in(11) ^ hash_in(7)  ^ hash_in(3)
							, hash_in(14) ^ hash_in(13) ^ hash_in(10) ^ hash_in(6)  ^ hash_in(2)
							, hash_in(13) ^ hash_in(12) ^ hash_in(9)  ^ hash_in(5)  ^ hash_in(1)
							, hash_in(12) ^ hash_in(11) ^ hash_in(8)  ^ hash_in(4)  ^ hash_in(0))
      crc
   }
	//yh+end
}

import ALU._

class ALU(implicit p: Parameters) extends CoreModule()(p) {
  val io = new Bundle {
    val valid = Bool(INPUT) //yh+
		val is_sp = Bool(INPUT) //yh+
    val dw = Bits(INPUT, SZ_DW)
    val fn = Bits(INPUT, SZ_ALU_FN)
    val in2 = UInt(INPUT, xLen)
    val in1 = UInt(INPUT, xLen)
    val out = UInt(OUTPUT, xLen)
    val adder_out = UInt(OUTPUT, xLen)
    val cmp_out = Bool(OUTPUT)
  }

  // ADD, SUB
  val in2_inv = Mux(isSub(io.fn), ~io.in2, io.in2)
  val in1_xor_in2 = io.in1 ^ in2_inv
  io.adder_out := io.in1 + in2_inv + isSub(io.fn)

  // SLT, SLTU
  val slt =
    Mux(io.in1(xLen-1) === io.in2(xLen-1), io.adder_out(xLen-1),
    Mux(cmpUnsigned(io.fn), io.in2(xLen-1), io.in1(xLen-1)))
  io.cmp_out := cmpInverted(io.fn) ^ Mux(cmpEq(io.fn), in1_xor_in2 === UInt(0), slt)

  // SLL, SRL, SRA
  val (shamt, shin_r) =
    if (xLen == 32) (io.in2(4,0), io.in1)
    else {
      require(xLen == 64)
      val shin_hi_32 = Fill(32, isSub(io.fn) && io.in1(31))
      val shin_hi = Mux(io.dw === DW_64, io.in1(63,32), shin_hi_32)
      val shamt = Cat(io.in2(5) & (io.dw === DW_64), io.in2(4,0))
      (shamt, Cat(shin_hi, io.in1(31,0)))
    }
  val shin = Mux(io.fn === FN_SR  || io.fn === FN_SRA, shin_r, Reverse(shin_r))
  val shout_r = (Cat(isSub(io.fn) & shin(xLen-1), shin).asSInt >> shamt)(xLen-1,0)
  val shout_l = Reverse(shout_r)
  val shout = Mux(io.fn === FN_SR || io.fn === FN_SRA, shout_r, UInt(0)) |
              Mux(io.fn === FN_SL,                     shout_l, UInt(0))

  // AND, OR, XOR
  val logic = Mux(io.fn === FN_XOR || io.fn === FN_OR, in1_xor_in2, UInt(0)) |
              Mux(io.fn === FN_OR || io.fn === FN_AND, io.in1 & io.in2, UInt(0))
  val shift_logic = (isCmp(io.fn) && slt) | logic | shout
  //yh-val out = Mux(io.fn === FN_ADD || io.fn === FN_SUB, io.adder_out, shift_logic)
  //yh+begin
	//val lfsr = RegInit(0x10.U(64.W))
  //val feedback = Wire(UInt(1.W))
  //feedback := (lfsr(0) ^ lfsr(1) ^ lfsr(3) ^ lfsr(4) ^ lfsr(20) ^ lfsr(26) ^
	//							lfsr(31) ^ lfsr(32) ^ lfsr(33) ^ lfsr(40) ^ lfsr(48) ^
	//							lfsr(54) ^ lfsr(56) ^ lfsr(59) ^ lfsr(60) ^ lfsr(63))
	val lfsr = RegInit(0x10.U(16.W))
  val feedback = Wire(UInt(1.W))
  feedback := (lfsr(5) ^ lfsr(3) ^ lfsr(2) ^ lfsr(0))
  lfsr := Mux(io.valid && io.fn === FN_TAGD, Cat(feedback, lfsr(15, 1)), lfsr)
  //lfsr := Cat(feedback, lfsr(15, 1))
	//when (io.valid && io.fn === FN_TAGD) {
	//	printf("lfsr: (0x%x -> 0x%x)\n", lfsr(15,0), Cat(feedback, lfsr(15, 1)))
	//}

	val crc0 = Mux(io.is_sp, lfsr(15,0), 0.U)
	val hash_in = (io.in1(47,0) ^ io.in2(63,0))
	val crc1 = genCrc16(hash_in(15,0) ^ crc0(15,0))
	val crc2 = genCrc16(hash_in(31,16) ^ crc1(15,0))
	val crc3 = genCrc16(hash_in(47,32) ^ crc2(15,0))
	val crc4 = genCrc16(hash_in(63,48) ^ crc3(15,0))
	val fin_crc = Mux(crc4 === 0.U, lfsr(15,0), crc4(15,0))

  //val temp = (io.in1(15,0) ^ io.in2(15,0))
  //val tag = Mux(temp === 0.U, 1.U, temp)
	val zero = Wire(UInt(16.W))
	zero := 0.U

  //val tag_out = Mux(io.fn === FN_TAGD, Cat(tag(15,0), io.in1(47,0)),
  val tag_out = Mux(io.fn === FN_TAGD, Cat(fin_crc(15,0), io.in1(47,0)),
                		Cat(zero(15,0), io.in1(47,0)))

  val out = Mux(io.fn === FN_ADD || io.fn === FN_SUB, io.adder_out,
                Mux(io.fn === FN_TAGD || io.fn === FN_XTAG, tag_out, shift_logic))

  when (io.valid && io.fn === FN_TAGD)
  {
    printf("YH+ Generated tag! io.in1: %x io.in2: %x io.is_sp: %d crc0: %x lfsr: %x tag_out: %x\n",
						io.in1, io.in2, io.is_sp, crc0, lfsr(15,0), tag_out)
  }
    .elsewhen (io.valid && io.fn === FN_XTAG)
  {
    printf("YH+ Removed tag! io.in1: %x tag_out: %x\n", io.in1, tag_out)
  }
  //yh+end

  io.out := out
  if (xLen > 32) {
    require(xLen == 64)
    when (io.dw === DW_32) { io.out := Cat(Fill(32, out(31)), out(31,0)) }
  }
}
