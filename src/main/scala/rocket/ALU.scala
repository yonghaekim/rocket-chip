// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util.{BitPat, Fill, Cat, Reverse}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.CoreModule

object ALU
{
  val SZ_ALU_FN = 4
  def FN_X    = BitPat("b????")
  def FN_ADD  = 0.U
  def FN_SL   = 1.U
  def FN_SEQ  = 2.U
  def FN_SNE  = 3.U
  def FN_XOR  = 4.U
  def FN_SR   = 5.U
  def FN_OR   = 6.U
  def FN_AND  = 7.U
  //yh+begin
  def FN_TAGC = 8.U
  def FN_XTAG = 9.U
  //yh+end
  def FN_SUB  = 10.U
  def FN_SRA  = 11.U
  def FN_SLT  = 12.U
  def FN_SGE  = 13.U
  def FN_SLTU = 14.U
  def FN_SGEU = 15.U

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
  val io = IO(new Bundle {
    val valid = Input(Bool()) //yh+
    val dw = Input(UInt(SZ_DW.W))
    val fn = Input(UInt(SZ_ALU_FN.W))
    val in2 = Input(UInt(xLen.W))
    val in1 = Input(UInt(xLen.W))
    val out = Output(UInt(xLen.W))
    val adder_out = Output(UInt(xLen.W))
    val cmp_out = Output(Bool())
  })

  // ADD, SUB
  val in2_inv = Mux(isSub(io.fn), ~io.in2, io.in2)
  val in1_xor_in2 = io.in1 ^ in2_inv
  io.adder_out := io.in1 + in2_inv + isSub(io.fn)

  // SLT, SLTU
  val slt =
    Mux(io.in1(xLen-1) === io.in2(xLen-1), io.adder_out(xLen-1),
    Mux(cmpUnsigned(io.fn), io.in2(xLen-1), io.in1(xLen-1)))
  io.cmp_out := cmpInverted(io.fn) ^ Mux(cmpEq(io.fn), in1_xor_in2 === 0.U, slt)

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
  val shout = Mux(io.fn === FN_SR || io.fn === FN_SRA, shout_r, 0.U) |
              Mux(io.fn === FN_SL,                     shout_l, 0.U)

  // AND, OR, XOR
  val logic = Mux(io.fn === FN_XOR || io.fn === FN_OR, in1_xor_in2, 0.U) |
              Mux(io.fn === FN_OR || io.fn === FN_AND, io.in1 & io.in2, 0.U)
  val shift_logic = (isCmp(io.fn) && slt) | logic | shout
  //yh-val out = Mux(io.fn === FN_ADD || io.fn === FN_SUB, io.adder_out, shift_logic)
  //yh+begin
	val hash_in = (io.in1(47,0) ^ io.in2(63,0))
	val crc1 = genCrc16(hash_in(15,0))
	val crc2 = genCrc16(hash_in(31,16) ^ crc1(15,0))
	val crc3 = genCrc16(hash_in(47,32) ^ crc2(15,0))
	val crc4 = genCrc16(hash_in(63,48) ^ crc3(15,0))
	val fin_crc = Mux(crc4 === 0.U, 1.U, crc4(15,0))

  //val temp = (io.in1(15,0) ^ io.in2(15,0))
  //val tag = Mux(temp === 0.U, 1.U, temp)
	val zero = Wire(UInt(16.W))
	zero := 0.U

  //val tag_out = Mux(io.fn === FN_TAGD, Cat(tag(15,0), io.in1(47,0)),
  val tag_out = Mux(io.fn === FN_TAGC, Cat(fin_crc(15,0), io.in1(47,0)),
                		Cat(zero(15,0), io.in1(47,0)))

  val out = Mux(io.fn === FN_ADD || io.fn === FN_SUB, io.adder_out,
                Mux(io.fn === FN_TAGC || io.fn === FN_XTAG, tag_out, shift_logic))

  when (io.valid && io.fn === FN_TAGC)
  {
    printf("YH+ Generated tag! io.in1: %x io.in2: %x tag_out: %x\n", io.in1, io.in2, tag_out)
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
