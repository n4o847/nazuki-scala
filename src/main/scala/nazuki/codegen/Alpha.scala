package nazuki.codegen

trait Alpha {
  trait Cmd
  case object Inc extends Cmd
  case object Dec extends Cmd
  case class Step(n: Int) extends Cmd
  case object Opn extends Cmd
  case object Cls extends Cmd
  case object Get extends Cmd
  case object Put extends Cmd

  var cmds = List.empty[Cmd]

  def bfNop() = {}

  def bfInc() = {
    cmds = cmds match {
      case Dec :: rest => rest
      case rest        => Inc :: rest
    }
  }

  def bfDec() = {
    cmds = cmds match {
      case Inc :: rest => rest
      case rest        => Dec :: rest
    }
  }

  def bfFwd() = bfStep(1)

  def bfBwd() = bfStep(-1)

  def bfStep(dx: Int) = {
    val (x, rest) = cmds match {
      case Step(x) :: rest => (x, rest)
      case rest            => (0, rest)
    }
    cmds =
      if (x + dx == 0)
        rest
      else
        Step(x + dx) :: rest
  }

  def bfOpn() = {
    cmds = Opn :: cmds
  }

  def bfCls() = {
    cmds = Cls :: cmds
  }

  def bfGet() = {
    cmds = Get :: cmds
  }

  def bfPut() = {
    cmds = Put :: cmds
  }

  def raw(code: String) = {
    for (c <- code) c match {
      case '+' => bfInc()
      case '-' => bfDec()
      case '>' => bfFwd()
      case '<' => bfBwd()
      case '[' => bfOpn()
      case ']' => bfCls()
      case ',' => bfGet()
      case '.' => bfPut()
      case _   => bfNop()
    }
  }

  def result: String = {
    val builder = new StringBuilder
    for (c <- cmds.reverse) {
      builder ++= (c match {
        case Inc     => "+"
        case Dec     => "-"
        case Step(n) => if (n >= 0) ">" * n else "<" * -n
        case Opn     => "["
        case Cls     => "]"
        case Get     => ","
        case Put     => "."
      })
    }
    builder.result
  }
}
