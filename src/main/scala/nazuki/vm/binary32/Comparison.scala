package nazuki.vm
package binary32

trait Comparison {
  self: Alpha with Beta with Stack =>

  private def doFlipLsb(): Unit = {
    consume(1)
    val a = alloc(0)
    val A = alloc(1 to 32)
    A(0) {
      A(0) -= 1
      a -= 1
    }
    a {
      a -= 1
      A(0) += 1
    }
    a += 1
    produce(1)
  }

  def doEqualToZero(): Unit = {
    doNotEqualToZero()
    doFlipLsb()
  }

  def doNotEqualToZero(): Unit = {
    consume(1)
    val a = alloc(0)
    val A = alloc(1 to 32)
    for (i <- 31 to 1 by -1) {
      A(i) {
        A(i) -= 1
        A(i - 1) := 1
      }
    }
    produce(1)
  }
}
