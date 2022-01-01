package nazuki.script

/** Based on
  *   - https://docs.python.org/3/library/ast.html
  *   - https://github.com/python/cpython/blob/3.10/Parser/Python.asdl
  */

object ast {
  sealed trait mod
  object mod {
    case class Module(body: Seq[stmt]) extends mod
  }

  sealed trait stmt
  object stmt {
    case class Assign(target: Seq[expr], value: expr) extends stmt
    case class AugAssign(target: expr, op: operator, value: expr) extends stmt
    case class While(test: expr, body: Seq[stmt]) extends stmt
    case class If(test: expr, body: Seq[stmt], orelse: Seq[stmt]) extends stmt
    case class Expr(value: expr) extends stmt
  }

  sealed trait expr
  object expr {
    case class BoolOp(op: boolop, values: Seq[expr]) extends expr
    case class BinOp(left: expr, op: operator, right: expr) extends expr
    case class UnaryOp(op: unaryop, operand: expr) extends expr
    case class Compare(left: expr, ops: Seq[cmpop], comparators: Seq[expr])
        extends expr
    case class Call(func: expr, args: Seq[expr]) extends expr
    case class Constant(value: constant) extends expr
    case class IntLit(value: Int) extends expr
    case class StringLit(value: String) extends expr
    case class CharLit(value: Char) extends expr
    case class Name(id: identifier, ctx: expr_context) extends expr
    case class Tuple(elts: Seq[expr], ctx: expr_context) extends expr
  }

  case class identifier(name: String)

  sealed trait constant
  object constant {
    case object True extends constant
    case object False extends constant
    case object None extends constant
    case object Ellipsis extends constant
  }

  sealed trait expr_context
  object expr_context {
    case object Load extends expr_context
    case object Store extends expr_context
  }

  sealed trait boolop
  object boolop {
    case object And extends boolop
    case object Or extends boolop
  }

  sealed trait operator
  object operator {
    case object Add extends operator
    case object Sub extends operator
    case object Mult extends operator
    case object MatMult extends operator
    case object Div extends operator
    case object Mod extends operator
    case object Pow extends operator
    case object LShift extends operator
    case object RShift extends operator
    case object BitOr extends operator
    case object BitXor extends operator
    case object BitAnd extends operator
    case object FloorDiv extends operator
  }

  sealed trait unaryop
  object unaryop {
    case object Invert extends unaryop
    case object Not extends unaryop
    case object UAdd extends unaryop
    case object USub extends unaryop
  }

  sealed trait cmpop
  object cmpop {
    case object Eq extends cmpop
    case object NotEq extends cmpop
    case object Lt extends cmpop
    case object LtE extends cmpop
    case object Gt extends cmpop
    case object GtE extends cmpop
  }
}
