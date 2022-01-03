package nazuki.codegen

case class Ptr(addr: Int, ctx: Beta) {
  def apply(f: => Unit) = {
    ctx.whileLoop(this) { f }
  }

  def +=(n: Int) = {
    ctx.add(this, n)
  }

  def -=(n: Int) = {
    ctx.sub(this, n)
  }

  def :=(n: Int) = {
    ctx.set(this, n)
  }
}

case class Vec(range: Range, ctx: Beta) {
  def apply(idx: Int) = Ptr(range(idx), ctx)
}

trait Beta {
  self: Alpha =>

  def alloc(addr: Int) = Ptr(addr, this)

  def alloc(range: Range) = Vec(range, this)

  def forward(n: Int) = {
    bfStep(n)
  }

  def backward(n: Int) = {
    bfStep(-n)
  }

  def enter(p: Ptr) = {
    forward(p.addr)
  }

  def exit(p: Ptr) = {
    backward(p.addr)
  }

  def at(p: Ptr)(f: => Unit) = {
    enter(p)
    f
    exit(p)
  }

  /** `p` の指す値を 1 増やす。 */
  def inc(p: Ptr) = {
    at(p) { bfInc() }
  }

  /** `p` の指す値を 1 減らす。 */
  def dec(p: Ptr) = {
    at(p) { bfDec() }
  }

  /** `p` の指す値を `n` 増やす。 */
  def add(p: Ptr, n: Int) = {
    at(p) {
      for (_ <- 0 until n) bfInc()
    }
  }

  /** `p` の指す値を `n` 減らす。 */
  def sub(p: Ptr, n: Int) = {
    at(p) {
      for (_ <- 0 until n) bfDec()
    }
  }

  def getc(p: Ptr) = {
    at(p) { bfGet() }
  }

  def putc(p: Ptr) = {
    at(p) { bfPut() }
  }

  def whileLoop(p: Ptr)(f: => Unit) = {
    at(p) { bfOpn() }
    f
    at(p) { bfCls() }
  }

  /** `p` の指す値を `n` にする。 */
  def set(p: Ptr, n: Int) = {
    whileLoop(p) {
      dec(p)
    }
    add(p, n)
  }

  /** 環境を破壊しないときのための分岐。
    *   - `temp` の指す値は 0 でなければならない。
    *   - `cond` の指す値が 1 のとき、
    *     - `doCons` を実行する。
    *     - このとき `cond` の指す値が変化してはいけない。
    *   - `cond` の指す値が 0 のとき、
    *     - `doAlt` を実行する。
    *     - このとき `cond` の指す値が変化してはいけない。
    */
  def branch(cond: Ptr, temp: Ptr)(doCons: => Unit)(doAlt: => Unit) = {
    cond {
      doCons
      temp -= 1
      cond -= 1
    }
    cond += 1
    temp += 1
    temp {
      temp -= 1
      cond -= 1
      doAlt
    }
  }

  /** 環境を破壊するときのための分岐。
    *   - `temp` の指す値は 0 でなければならない。
    *   - `cond` の指す値が 1 のとき、
    *     - `doCons` を実行する。
    *     - このとき `cond` の指す値が変化しても、`doAlt` は実行されない。
    *   - `cond` の指す値が 0 のとき、
    *     - `doAlt` を実行する。
    *     - このとき `cond` の指す値が変化しても、`doCons` は実行されない。
    */
  def branchMut(cond: Ptr, temp: Ptr)(doCons: => Unit)(doAlt: => Unit) = {
    cond {
      doCons
      temp -= 1
      cond {
        cond -= 1
        temp -= 1
      }
    }
    cond += 1
    temp += 2
    temp {
      temp -= 1
      cond -= 1
      temp {
        temp -= 1
        doAlt
      }
    }
  }
}
