package nazuki.vm
package binary32

trait Control {
  self: Alpha with Beta with Stack with Comparison =>

  def jump(target: Int): Unit

  def doJump(target: Int): Unit = {
    jump(target)
  }

  private def doJumpOnLsb(target: Int): Unit = {
    consume(1)
    val a = alloc(0)
    val A0 = alloc(1)
    a -= 1
    A0 {
      A0 -= 1
      jump(target)
    }
    produce(0)
  }

  def doBranchOnZero(target: Int): Unit = {
    doEqualToZero()
    doJumpOnLsb(target)
  }

  def doBranchOnNonZero(target: Int): Unit = {
    doNotEqualToZero()
    doJumpOnLsb(target)
  }
}
