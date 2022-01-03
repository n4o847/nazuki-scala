package nazuki

package object util {
  trait Bits {
    def testBit(i: Int): Boolean
  }

  implicit class IntBits(x: Int) extends Bits {
    def testBit(i: Int) = {
      ((x >> i) & 1) == 1
    }
  }

  implicit class RichInt(value: Int) {
    def downto(end: Int) = {
      Range.inclusive(value, end, -1)
    }

    def times(f: => Unit) = {
      for (_ <- 1 to value) {
        f
      }
    }
  }

  implicit class RichBigInt(value: BigInt) {
    def digits: List[Int] = {
      if (value == 0) {
        Nil
      } else {
        (value % 10).toInt :: (value / 10).digits
      }
    }
  }
}
