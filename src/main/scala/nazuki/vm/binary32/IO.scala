package nazuki.vm
package binary32

import nazuki.util._

trait IO {
  self: Alpha with Beta with Stack =>

  def doScan(signed: Boolean) = {
    consume(0)

    val a = alloc(0)
    val A = Int32(alloc(1 to 33))
    val t = alloc(33) // temporary
    val v = alloc(33 + 1) // value of the digit
    val l = alloc(33 + 2) // to continue the loop?
    val n = alloc(33 + 3) // is negative?

    // if ('0' <= t <= '9') { v := t - 48 }
    // else { v := 0; l := false }
    def evalDigit() = {
      t -= 48
      t {
        for (_ <- 1 to 9) {
          t -= 1
          v += 1
          at(t) { bfOpn() }
        }
        t := 0
        v := 0
        l -= 1
        for (_ <- 1 to 9) {
          at(t) { bfCls() }
        }
      }
    }

    def addDigit() = {
      v {
        v -= 1
        A.increment()
        A.carry := 0
      }
    }

    // check if starting with '-'
    if (signed) {
      n += 1
      getc(t)
      t -= 45 // '-'
      t {
        n -= 1
        t += 45
        evalDigit()
        addDigit()
      }
    }
    // main
    l += 1
    l {
      getc(t)
      evalDigit()
      l {
        l -= 1
        A.decuple()
        t += 1
      }
      t {
        t -= 1
        l += 1
      }
      addDigit()
    }
    a += 1
    // negate
    if (signed) {
      n {
        n -= 1
        A.invert()
        a -= 1
        A.increment()
        a += 1
        A.carry := 0
      }
    }
    produce(1)
  }

  def doPrint(signed: Boolean) = {
    consume(1)
    val a = alloc(0)
    val A = Int32(alloc(1 to 33))
    if (signed) {
      val t0 = alloc(33)
      val t1 = alloc(33 + 1) // is negative?
      A(31) {
        t0 += 45 // '-'
        putc(t0)
        t0 := 0
        A.invert()
        a -= 1
        A.increment()
        a += 1
        A(31) {
          A(31) -= 1
          t1 += 1
        }
      }
      t1 {
        t1 -= 1
        A(31) += 1
      }
    } // end if
    for (i <- 0 to 31) {
      A(i) {
        A(i) -= 1
        for ((d, j) <- (BigInt(1) << i).digits.zipWithIndex) {
          val t = alloc(33 + 3 * j)
          t += d
        }
      }
    }
    for (j <- 0 to 8) {
      val t0 = alloc(33 + 3 * j) // dividend
      val t1 = alloc(33 + 3 * j + 1) // divisor
      val t2 = alloc(33 + 3 * j + 2) // remainder
      val u0 = alloc(33 + 3 * j + 3) // quotient
      t1 += 10
      at(t0) {
        // https://esolangs.org/wiki/Brainfuck_algorithms#Divmod_algorithm
        // >n d
        raw("[->-[>+>>]>[+[-<+>]>+>>]<<<<<]")
        // >0 d-n%d n%d n/d
      }
      t1 := 0
      t2 {
        t2 -= 1
        t0 += 1
      }
    }
    for (j <- 9 to 1 by -1) {
      val s1 = alloc(33 + 3 * j - 2)
      val t0 = alloc(33 + 3 * j) // digit
      val t1 = alloc(33 + 3 * j + 1) // to print?
      val t2 = alloc(33 + 3 * j + 2)
      t0 {
        t0 -= 1
        t1 += 1
        t2 += 1
      }
      t1 {
        t1 { //
          t1 := 0
          s1 += 1
        } //
        t2 += 48
        putc(t2)
        t2 := 0
      }
    }
    {
      val t0 = alloc(33)
      val t1 = alloc(33 + 1)
      t1 := 0
      t0 += 48
      putc(t0)
      t0 := 0
    }
    a -= 1
    produce(0)
  }
}
