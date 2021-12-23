package nazuki.codegen

import nazuki.util._

trait Stack {
  self: Alpha with Beta =>

  def consume(n: Int) = {
    backward(33 * n)
  }

  def produce(n: Int) = {
    forward(33 * n)
  }

  def doConst(x: Int) = {
    consume(0)
    val a = alloc(0)
    val A = alloc(1 to 32)
    a += 1
    for (i <- 0 to 31) {
      if (x.testBit(i)) {
        A(i) += 1
      }
    }
    produce(1)
  }

  private def toFront = {
    backward(33)
    bfOpn
    backward(33)
    bfCls
  }

  private def toBack = {
    forward(33)
    bfOpn
    forward(33)
    bfCls
  }

  def doGet(x: Int) = {
    // Non-negative indices count from the front of the stack.
    if (x >= 0) {
      consume(0)
      val b = alloc(0)
      val B = alloc(1 to 32)
      toFront
      forward(33)
      for (_ <- 1 to x) {
        bfDec
        forward(33)
      }
      bfDec
      val a = alloc(0)
      val A = alloc(1 to 32)
      for (i <- 0 to 31) {
        A(i) {
          A(i) -= 1
          toBack
          B(i) += 1
          toFront
          a += 1
        }
        a {
          a -= 1
          A(i) += 1
        }
      }
      bfInc
      for (_ <- 1 to x) {
        backward(33)
        bfInc
      }
      toBack
      b += 1
      produce(1)
    }
    // Negative indices count from the back of the stack.
    else {
      consume(0)
      val a = alloc(33 * x)
      val A = alloc(33 * x + 1 to 33 * x + 32)
      val b = alloc(0)
      val B = alloc(1 to 32)
      for (i <- 0 to 31) {
        A(i) {
          A(i) -= 1
          b += 1
          B(i) += 1
        }
        b {
          b -= 1
          A(i) += 1
        }
      }
      b += 1
      produce(1)
    }
  }

  def doSet(x: Int) = {
    // Non-negative indices count from the front of the stack.
    if (x >= 0) {
      consume(1)
      val b = alloc(0)
      val B = alloc(1 to 32)
      b -= 1
      toFront
      forward(33)
      for (_ <- 1 to x) {
        bfDec
        forward(33)
      }
      bfDec
      val a = alloc(0)
      val A = alloc(1 to 32)
      for (i <- 0 to 31) {
        A(i) {
          A(i) -= 1
        }
      }
      toBack
      for (i <- 0 to 31) {
        B(i) {
          B(i) -= 1
          toFront
          A(i) += 1
          toBack
        }
      }
      toFront
      bfInc
      for (_ <- 1 to x) {
        backward(33)
        bfInc
      }
      backward(33)
      toBack
      produce(0)
    }
    // Negative indices count from the back of the stack.
    else {
      consume(1)
      val a = alloc(33 * x)
      val A = alloc(33 * x + 1 to 33 * x + 32)
      val b = alloc(0)
      val B = alloc(1 to 32)
      b -= 1
      for (i <- 0 to 31) {
        A(i) {
          A(i) -= 1
        }
        B(i) {
          B(i) -= 1
          A(i) += 1
        }
      }
      produce(0)
    }
  }
}
