package nazuki.codegen
package binary32

import nazuki.util._

trait Bitwise {
  self: Alpha with Beta with Stack =>

  def doNot() = {
    consume(1)
    val A = alloc(1 to 32)
    val B = alloc(2 to 33)
    for (i <- 31 to 0 by -1) {
      B(i) += 1
      A(i) {
        A(i) -= 1
        B(i) -= 1
      }
    }
    for (i <- 0 to 31) {
      B(i) {
        B(i) -= 1
        A(i) += 1
      }
    }
    produce(1)
  }

  def doAnd() = {
    consume(2)
    val A = alloc(1 to 32)
    val b = alloc(33)
    val B = alloc(34 to 65)
    for (i <- 31 to 0 by -1) {
      B(i) -= 1
      B(i) {
        B(i) += 1
        A(i) := 0
      }
    }
    b -= 1
    produce(1)
  }

  def doOr() = {
    consume(2)
    val A = alloc(1 to 32)
    val b = alloc(33)
    val B = alloc(34 to 65)
    for (i <- 31 to 0 by -1) {
      B(i) {
        B(i) -= 1
        A(i) := 1
      }
    }
    b -= 1
    produce(1)
  }
}
