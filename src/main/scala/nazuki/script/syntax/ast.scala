package nazuki.script.syntax

/** Based on
  *   - https://docs.python.org/3/library/ast.html
  *   - https://github.com/python/cpython/blob/3.10/Parser/Python.asdl
  */

object ast {
  enum mod {
    case Module(body: Seq[stmt])
  }

  enum stmt {
    case Assign(target: Seq[expr], value: expr)
    case AugAssign(target: expr, op: operator, value: expr)
    case While(test: expr, body: Seq[stmt])
    case If(test: expr, body: Seq[stmt], orelse: Seq[stmt])
    case Expr(value: expr)
  }

  enum expr {
    case BoolOp(op: boolop, values: Seq[expr])
    case BinOp(left: expr, op: operator, right: expr)
    case UnaryOp(op: unaryop, operand: expr)
    case Compare(left: expr, ops: Seq[cmpop], comparators: Seq[expr])
    case Call(func: expr, args: Seq[expr])
    case Constant(value: constant)
    case IntLit(value: Int)
    case StringLit(value: String)
    case CharLit(value: Char)
    case Name(id: identifier, ctx: expr_context)
    case Tuple(elts: Seq[expr], ctx: expr_context)
  }

  case class identifier(name: String)

  enum constant {
    case True
    case False
    case None
    case Ellipsis
  }

  enum expr_context {
    case Load
    case Store
  }

  enum boolop {
    case And
    case Or
  }

  enum operator {
    case Add
    case Sub
    case Mult
    case MatMult
    case Div
    case Mod
    case Pow
    case LShift
    case RShift
    case BitOr
    case BitXor
    case BitAnd
    case FloorDiv
  }

  enum unaryop {
    case Invert
    case Not
    case UAdd
    case USub
  }

  enum cmpop {
    case Eq
    case NotEq
    case Lt
    case LtE
    case Gt
    case GtE
  }
}
