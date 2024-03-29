package nazuki.vm
package binary32

case class Int32(range: Vec)(using alpha: Alpha, beta: Beta) {
  def carry = range(32)

  def apply(idx: Int) = range(idx)

  def invert() = {
    val A = this
    for (i <- 31 to 0 by -1) {
      A(i + 1) += 1
      A(i) {
        A(i) -= 1
        A(i + 1) -= 1
      }
    }
    for (i <- 0 to 31) {
      A(i + 1) {
        A(i + 1) -= 1
        A(i) += 1
      }
    }
  }

  def increment(lsb: Int = 0) = {
    beta.at(range(lsb)) {
      alpha.raw("[>]+<[-<]>")
    }
  }

  def decuple() = {
    val A = this
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
  }
}
