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

  //protected def boundsMarginCSRId = 0x431
  //protected def boundsMarginCSR: Option[CustomCSR] = None

  protected def numTagdCSRId = 0x432
  protected def numTagdCSR: Option[CustomCSR] = None

  protected def numXtagCSRId = 0x433
  protected def numXtagCSR: Option[CustomCSR] = None

  protected def numTaggedStoreCSRId = 0x434
  protected def numTaggedStoreCSR: Option[CustomCSR] = None

  protected def numUntaggedStoreCSRId = 0x435
  protected def numUntaggedStoreCSR: Option[CustomCSR] = None

  protected def numTaggedLoadCSRId = 0x436
  protected def numTaggedLoadCSR: Option[CustomCSR] = None

  protected def numUntaggedLoadCSRId = 0x437
  protected def numUntaggedLoadCSR: Option[CustomCSR] = None

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

  //protected def numCstrCSRId = 0x43d
  //protected def numCstrCSR: Option[CustomCSR] = None

  //protected def numCclrCSRId = 0x43e
  //protected def numCclrCSR: Option[CustomCSR] = None

  //protected def numCsrchCSRId = 0x43f
  //protected def numCsrchCSR: Option[CustomCSR] = None

  //protected def numCsrchHitCSRId = 0x440
  //protected def numCsrchHitCSR: Option[CustomCSR] = None

  //protected def numCstrItrCSRId = 0x441
  //protected def numCstrItrCSR: Option[CustomCSR] = None

  //protected def numCclrItrCSRId = 0x442
  //protected def numCclrItrCSR: Option[CustomCSR] = None

  //protected def numCsrchItrCSRId = 0x443
  //protected def numCsrchItrCSR: Option[CustomCSR] = None

  //protected def numChkFailCSRId = 0x444
  //protected def numChkFailCSR: Option[CustomCSR] = None

  //protected def numCstrFailCSRId = 0x445
  //protected def numCstrFailCSR: Option[CustomCSR] = None

  //protected def numCclrFailCSRId = 0x446
  //protected def numCclrFailCSR: Option[CustomCSR] = None

  def dpt_config       	  = getOrElse(dptConfigCSR, _.value, UInt(xLen.W))
  //def bounds_margin			  = getOrElse(boundsMarginCSR, _.value, UInt(xLen.W))
	def num_tagd						= getOrElse(numTagdCSR, _.value, UInt(xLen.W))
	def num_xtag						= getOrElse(numXtagCSR, _.value, UInt(xLen.W))
  def num_tagged_store    = getOrElse(numTaggedStoreCSR, _.value, UInt(xLen.W))
  def num_untagged_store  = getOrElse(numUntaggedStoreCSR, _.value, UInt(xLen.W))
  def num_tagged_load     = getOrElse(numTaggedLoadCSR, _.value, UInt(xLen.W))
  def num_untagged_load   = getOrElse(numUntaggedLoadCSR, _.value, UInt(xLen.W))
	def num_inst						= getOrElse(numInstCSR, _.value, UInt(xLen.W))
  def ldst_traffic        = getOrElse(ldstTrafficCSR, _.value, UInt(xLen.W))
  def bounds_traffic      = getOrElse(boundsTrafficCSR, _.value, UInt(xLen.W))
  def num_store_hit       = getOrElse(numStoreHitCSR, _.value, UInt(xLen.W))
  def num_load_hit        = getOrElse(numLoadHitCSR, _.value, UInt(xLen.W))
	//def num_cstr						= getOrElse(numCstrCSR, _.value, UInt(xLen.W))
	//def num_cclr						= getOrElse(numCclrCSR, _.value, UInt(xLen.W))
	//def num_csrch						= getOrElse(numCsrchCSR, _.value, UInt(xLen.W))
	//def num_csrch_hit				= getOrElse(numCsrchHitCSR, _.value, UInt(xLen.W))
	//def num_cstr_itr				= getOrElse(numCstrItrCSR, _.value, UInt(xLen.W))
	//def num_cclr_itr				= getOrElse(numCclrItrCSR, _.value, UInt(xLen.W))
	//def num_csrch_itr				= getOrElse(numCsrchItrCSR, _.value, UInt(xLen.W))
	//def num_chk_fail				= getOrElse(numChkFailCSR, _.value, UInt(xLen.W))
	//def num_cstr_fail				= getOrElse(numCstrFailCSR, _.value, UInt(xLen.W))
	//def num_cclr_fail				= getOrElse(numCclrFailCSR, _.value, UInt(xLen.W))

  // If you override this, you'll want to concatenate super.decls
  def decls: Seq[CustomCSR] = bpmCSR.toSeq ++ chickenCSR ++ dptConfigCSR ++ //boundsMarginCSR ++
																numTagdCSR ++ numXtagCSR ++
																numTaggedStoreCSR ++ numUntaggedStoreCSR ++
																numTaggedLoadCSR ++ numUntaggedLoadCSR ++
																numInstCSR ++
                                ldstTrafficCSR ++ boundsTrafficCSR ++
                                numStoreHitCSR ++ numLoadHitCSR 
																//numCstrCSR ++ numCclrCSR ++ numCsrchCSR
                                //++ numChkFailCSR
                                //numCsrchHitCSR ++
                                //numCstrItrCSR ++ numCclrItrCSR ++ numCsrchItrCSR ++
                                //numChkFailCSR ++ numCstrFailCSR ++ numCclrFailCSR
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
