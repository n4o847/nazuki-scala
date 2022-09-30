package nazuki.vm
package binary32

trait Arithmetic {
  self: Alpha with Beta with Stack =>

  def doInc() = {
    consume(1)
    val a = alloc(0)
    val A = Int32(alloc(1 to 33))
    a -= 1
    A.increment()
    a += 1
    A.carry := 0
    produce(1)
  }

  def doAdd() = {
    consume(2)
    val a = alloc(0)
    val A = Int32(alloc(1 to 33))
    val b = alloc(33)
    val B = Int32(alloc(34 to 66))
    b -= 1
    for (i <- 0 to 31) {
      A(i) {
        A(i) -= 1
        B.increment(i)
      }
      B(i) {
        B(i) -= 1
        A(i) += 1
      }
    }
    B.carry := 0
    produce(1)
  }

  def doMul10() = {
    consume(1)
    val a = alloc(0)
    val A = Int32(alloc(1 to 33))
    A(31) := 0
    A(30) {
      A(30) -= 1
      A(31) += 1
    }
    A(29) {
      A(29) -= 1
      A(30) += 1
    }
    for (i <- 28 to 0 by -1) {
      A(i) {
        A(i) -= 1
        A(i + 2) {
          A(i + 2) -= 1
          A(i + 1) += 1
        }
        A.increment(i + 3)
        A(i + 1) {
          A(i + 1) -= 1
          A(i + 2) += 1
        }
        A(i + 1) += 1
      }
    }
    A.carry := 0
    produce(1)
  }
}
