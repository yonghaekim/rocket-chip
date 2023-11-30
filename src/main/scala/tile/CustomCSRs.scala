// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import chisel3._

import freechips.rocketchip.config.Parameters

case class CustomCSR(id: Int, mask: BigInt, init: Option[BigInt])

object CustomCSR {
  def constant(id: Int, value: BigInt): CustomCSR = CustomCSR(id, BigInt(0), Some(value))
}

class CustomCSRIO(implicit p: Parameters) extends CoreBundle {
  val wen = Bool()
  val wdata = UInt(xLen.W)
  val value = UInt(xLen.W)
}

class CustomCSRs(implicit p: Parameters) extends CoreBundle {
  // Not all cores have these CSRs, but those that do should follow the same
  // numbering conventions.  So we list them here but default them to None.
  protected def bpmCSRId = 0x7c0
  protected def bpmCSR: Option[CustomCSR] = None

  protected def chickenCSRId = 0x7c1
  protected def chickenCSR: Option[CustomCSR] = None

  // If you override this, you'll want to concatenate super.decls
  //yh-def decls: Seq[CustomCSR] = bpmCSR.toSeq ++ chickenCSR

  //yh+begin
  protected def dptConfigCSRId = 0x430
  protected def dptConfigCSR: Option[CustomCSR] = None

  protected def wpbBaseCSRId = 0x431
  protected def wpbBaseCSR: Option[CustomCSR] = None

  protected def numTagdCSRId = 0x432
  protected def numTagdCSR: Option[CustomCSR] = None

  protected def numXtagCSRId = 0x433
  protected def numXtagCSR: Option[CustomCSR] = None

  protected def numStoreCSRId = 0x434
  protected def numStoreCSR: Option[CustomCSR] = None

  protected def numLoadCSRId = 0x435
  protected def numLoadCSR: Option[CustomCSR] = None

  protected def numTaggedStoreCSRId = 0x436
  protected def numTaggedStoreCSR: Option[CustomCSR] = None

  protected def numTaggedLoadCSRId = 0x437
  protected def numTaggedLoadCSR: Option[CustomCSR] = None

  protected def numInstCSRId = 0x438
  protected def numInstCSR: Option[CustomCSR] = None

  protected def ldstTrafficCSRId = 0x439
  protected def ldstTrafficCSR: Option[CustomCSR] = None

  protected def boundsTrafficCSRId = 0x43a
  protected def boundsTrafficCSR: Option[CustomCSR] = None

  protected def numStoreHitCSRId = 0x43b
  protected def numStoreHitCSR: Option[CustomCSR] = None

  protected def numLoadHitCSRId = 0x43c
  protected def numLoadHitCSR: Option[CustomCSR] = None

  protected def numCstrCSRId = 0x43d
  protected def numCstrCSR: Option[CustomCSR] = None

  protected def numCclrCSRId = 0x43e
  protected def numCclrCSR: Option[CustomCSR] = None

  protected def boundsMarginCSRId = 0x43f
  protected def boundsMarginCSR: Option[CustomCSR] = None

  protected def arenaEnd0CSRId = 0x440
  protected def arenaEnd0CSR: Option[CustomCSR] = None

  protected def arenaEnd1CSRId = 0x441
  protected def arenaEnd1CSR: Option[CustomCSR] = None

  protected def arenaEnd2CSRId = 0x442
  protected def arenaEnd2CSR: Option[CustomCSR] = None

  protected def arenaEnd3CSRId = 0x443
  protected def arenaEnd3CSR: Option[CustomCSR] = None

  protected def arenaEnd4CSRId = 0x444
  protected def arenaEnd4CSR: Option[CustomCSR] = None

  protected def arenaEnd5CSRId = 0x445
  protected def arenaEnd5CSR: Option[CustomCSR] = None

  protected def arenaEnd6CSRId = 0x446
  protected def arenaEnd6CSR: Option[CustomCSR] = None

  protected def arenaEnd7CSRId = 0x447
  protected def arenaEnd7CSR: Option[CustomCSR] = None

  protected def numWays0CSRId = 0x448
  protected def numWays0CSR: Option[CustomCSR] = None

  protected def numWays1CSRId = 0x449
  protected def numWays1CSR: Option[CustomCSR] = None

  protected def numWays2CSRId = 0x44a
  protected def numWays2CSR: Option[CustomCSR] = None

  protected def numWays3CSRId = 0x44b
  protected def numWays3CSR: Option[CustomCSR] = None

  protected def numSlqItrCSRId = 0x44c
  protected def numSlqItrCSR: Option[CustomCSR] = None

  protected def numSsqItrCSRId = 0x44d
  protected def numSsqItrCSR: Option[CustomCSR] = None

  protected def numScqItrCSRId = 0x44e
  protected def numScqItrCSR: Option[CustomCSR] = None

  def dpt_config       	  = getOrElse(dptConfigCSR, _.value, UInt(xLen.W))
  def wpb_base			  		= getOrElse(wpbBaseCSR, _.value, UInt(xLen.W))
	def num_tagd						= getOrElse(numTagdCSR, _.value, UInt(xLen.W))
	def num_xtag						= getOrElse(numXtagCSR, _.value, UInt(xLen.W))
  def num_store           = getOrElse(numStoreCSR, _.value, UInt(xLen.W))
  def num_load            = getOrElse(numLoadCSR, _.value, UInt(xLen.W))
  def num_tagged_store    = getOrElse(numTaggedStoreCSR, _.value, UInt(xLen.W))
  def num_tagged_load     = getOrElse(numTaggedLoadCSR, _.value, UInt(xLen.W))
	def num_inst						= getOrElse(numInstCSR, _.value, UInt(xLen.W))
  def ldst_traffic        = getOrElse(ldstTrafficCSR, _.value, UInt(xLen.W))
  def bounds_traffic      = getOrElse(boundsTrafficCSR, _.value, UInt(xLen.W))
  def num_store_hit       = getOrElse(numStoreHitCSR, _.value, UInt(xLen.W))
  def num_load_hit        = getOrElse(numLoadHitCSR, _.value, UInt(xLen.W))
	def num_cstr						= getOrElse(numCstrCSR, _.value, UInt(xLen.W))
	def num_cclr						= getOrElse(numCclrCSR, _.value, UInt(xLen.W))
	def bounds_margin				= getOrElse(boundsMarginCSR, _.value, UInt(xLen.W))
	def arena_end_0				  = getOrElse(arenaEnd0CSR, _.value, UInt(xLen.W))
	def arena_end_1				  = getOrElse(arenaEnd1CSR, _.value, UInt(xLen.W))
	def arena_end_2				  = getOrElse(arenaEnd2CSR, _.value, UInt(xLen.W))
	def arena_end_3				  = getOrElse(arenaEnd3CSR, _.value, UInt(xLen.W))
	def arena_end_4				  = getOrElse(arenaEnd4CSR, _.value, UInt(xLen.W))
	def arena_end_5				  = getOrElse(arenaEnd5CSR, _.value, UInt(xLen.W))
	def arena_end_6				  = getOrElse(arenaEnd6CSR, _.value, UInt(xLen.W))
	def arena_end_7				  = getOrElse(arenaEnd7CSR, _.value, UInt(xLen.W))
	def num_ways_0				  = getOrElse(numWays0CSR, _.value, UInt(xLen.W))
	def num_ways_1				  = getOrElse(numWays1CSR, _.value, UInt(xLen.W))
	def num_ways_2				  = getOrElse(numWays2CSR, _.value, UInt(xLen.W))
	def num_ways_3				  = getOrElse(numWays3CSR, _.value, UInt(xLen.W))
	def num_slq_itr 			  = getOrElse(numSlqItrCSR, _.value, UInt(xLen.W))
	def num_ssq_itr 			  = getOrElse(numSsqItrCSR, _.value, UInt(xLen.W))
	def num_scq_itr 			  = getOrElse(numScqItrCSR, _.value, UInt(xLen.W))

  // If you override this, you'll want to concatenate super.decls
  def decls: Seq[CustomCSR] = bpmCSR.toSeq ++ chickenCSR ++ dptConfigCSR ++ wpbBaseCSR ++
																numTagdCSR ++ numXtagCSR ++ numStoreCSR ++ numLoadCSR ++
																numTaggedStoreCSR ++ numTaggedLoadCSR ++ numInstCSR ++
                                ldstTrafficCSR ++ boundsTrafficCSR ++
                                numStoreHitCSR ++ numLoadHitCSR ++ numCstrCSR ++ numCclrCSR ++ boundsMarginCSR ++
                                arenaEnd0CSR ++ arenaEnd1CSR ++ arenaEnd2CSR ++ arenaEnd3CSR ++ 
                                arenaEnd4CSR ++ arenaEnd5CSR ++ arenaEnd6CSR ++ arenaEnd7CSR ++ 
                                numWays0CSR ++ numWays1CSR ++ numWays2CSR ++ numWays3CSR ++
                                numSlqItrCSR ++ numSsqItrCSR ++ numScqItrCSR
  //yh+end

  val csrs = Vec(decls.size, new CustomCSRIO)

  def flushBTB = getOrElse(bpmCSR, _.wen, false.B)
  def bpmStatic = getOrElse(bpmCSR, _.value(0), false.B)
  def disableDCacheClockGate = getOrElse(chickenCSR, _.value(0), false.B)
  def disableICacheClockGate = getOrElse(chickenCSR, _.value(1), false.B)
  def disableCoreClockGate = getOrElse(chickenCSR, _.value(2), false.B)
  def disableSpeculativeICacheRefill = getOrElse(chickenCSR, _.value(3), false.B)
  def suppressCorruptOnGrantData = getOrElse(chickenCSR, _.value(9), false.B)

  protected def getByIdOrElse[T](id: Int, f: CustomCSRIO => T, alt: T): T = {
    val idx = decls.indexWhere(_.id == id)
    if (idx < 0) alt else f(csrs(idx))
  }

  protected def getOrElse[T](csr: Option[CustomCSR], f: CustomCSRIO => T, alt: T): T =
    csr.map(c => getByIdOrElse(c.id, f, alt)).getOrElse(alt)
}
