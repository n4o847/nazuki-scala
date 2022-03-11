package nazuki.script.syntax

/** Based on
  *   - https://docs.python.org/3/library/ast.html
  *   - https://github.com/python/cpython/blob/3.10/Parser/Python.asdl
  */

object ast {
  enum Mod {
    case Module(body: Seq[Stmt])
  }

  enum Stmt {
    case Assign(target: Seq[ast.Expr], value: ast.Expr)
    case AugAssign(target: ast.Expr, op: Operator, value: ast.Expr)
    case While(test: ast.Expr, body: Seq[Stmt])
    case If(test: ast.Expr, body: Seq[Stmt], orelse: Seq[Stmt])
    case Expr(value: ast.Expr)
  }

  enum Expr {
    case BoolOp(op: Boolop, values: Seq[Expr])
    case BinOp(left: Expr, op: Operator, right: Expr)
    case UnaryOp(op: Unaryop, operand: Expr)
    case Compare(left: Expr, ops: Seq[Cmpop], comparators: Seq[Expr])
    case Call(func: Expr, args: Seq[Expr])
    case Constant(value: ast.Constant)
    case IntLit(value: Int)
    case StringLit(value: String)
    case CharLit(value: Char)
    case Name(id: Identifier, ctx: ExprContext)
    case Tuple(elts: Seq[Expr], ctx: ExprContext)
  }

  case class Identifier(name: String)

  enum Constant {
    case True
    case False
    case None
    case Ellipsis
  }

  enum ExprContext {
    case Load
    case Store
  }

  enum Boolop {
    case And
    case Or
  }

  enum Operator {
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

  enum Unaryop {
    case Invert
    case Not
    case UAdd
    case USub
  }

  enum Cmpop {
    case Eq
    case NotEq
    case Lt
    case LtE
    case Gt
    case GtE
  }
}
