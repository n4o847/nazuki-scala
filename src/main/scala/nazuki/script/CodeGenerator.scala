package nazuki.script

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

import nazuki.assembly.Instruction
import nazuki.assembly.Instruction as I
import nazuki.assembly.Labeled
import syntax.ast._

import Labeled._

final case class CodeGenerationException(
    message: String
) extends Exception(message)

class CodeGenerator {
  val instructions = ArrayBuffer.empty[Labeled[Instruction]]
  val environment = Map.empty[Identifier, Tuple2[Type, Int]]
  var nextLabel = 0

  def createLabel(prefix: String): String = {
    val label = s"${prefix}${nextLabel}"
    nextLabel += 1
    label
  }

  def fail(message: String): Nothing = {
    throw new CodeGenerationException(message)
  }

  def fromModule(mod: Mod): Unit = {
    mod match {
      case Mod.Module(body) =>
        for (stmt <- body) {
          fromStmt(stmt)
        }
    }
  }

  def fromStmt(stmt: Stmt): Unit = {
    stmt match {
      case stmt: Stmt.Assign    => fromStmtAssign(stmt)
      case stmt: Stmt.AugAssign => fromStmtAugAssign(stmt)
      case stmt: Stmt.While     => fromStmtWhile(stmt)
      case stmt: Stmt.If        => fromStmtIf(stmt)
      case stmt: Stmt.Expr      => fromStmtExpr(stmt)
    }
  }

  def fromStmtAssign(stmt: Stmt.Assign): Unit = {
    val t0 = fromExpr(stmt.value)
    stmt.target match {
      case Expr.Name(ident, ExprContext.Store) :: Nil =>
        environment.get(ident) match {
          case Some((t1, index)) =>
            if (t0 != t1) {
              fail("mismatched types")
            }
            instructions += L0(I.Set(index))
          case None =>
            val index = environment.size
            environment.update(ident, (t0, index))
        }
      case _ =>
        fail("invalid types")
    }
  }

  def fromStmtAugAssign(stmt: Stmt.AugAssign): Unit = {
    stmt.target match {
      case Expr.Name(ident, ExprContext.Store) =>
        fromStmtAssign(
          Stmt.Assign(
            List(stmt.target),
            Expr.BinOp(Expr.Name(ident, ExprContext.Load), stmt.op, stmt.value)
          )
        )
      case _ =>
        ???
    }
  }

  def fromStmtWhile(stmt: Stmt.While): Unit = {
    val lStart = createLabel("L")
    val lEnd = createLabel("L")
    instructions += Label(lStart)
    val t0 = fromExpr(stmt.test)
    if (t0 != Type.Int) {
      fail("invalid types")
    }
    instructions += L1(I.Jz(_), lEnd)
    for (stmt <- stmt.body) {
      fromStmt(stmt)
    }
    instructions ++= List(
      L1(I.Jump(_), lStart),
      Label(lEnd)
    )
  }

  def fromStmtIf(stmt: Stmt.If): Unit = {
    stmt.orelse match {
      case Nil =>
        val lEnd = createLabel("L")
        val t0 = fromExpr(stmt.test)
        if (t0 != Type.Int) {
          fail("invalid types")
        }
        instructions += L1(I.Jz(_), lEnd)
        for (stmt <- stmt.body) {
          fromStmt(stmt)
        }
        instructions += Label(lEnd)
      case _ =>
        val lElse = createLabel("L")
        val lEnd = createLabel("L")
        val t0 = fromExpr(stmt.test)
        if (t0 != Type.Int) {
          fail("invalid types")
        }
        instructions += L1(I.Jz(_), lElse)
        for (stmt <- stmt.body) {
          fromStmt(stmt)
        }
        instructions ++= List(
          L1(I.Jump(_), lEnd),
          Label(lElse)
        )
        for (stmt <- stmt.orelse) {
          fromStmt(stmt)
        }
        instructions += Label(lEnd)
    }
  }

  def fromStmtExpr(stmt: Stmt.Expr): Unit = {
    val t0 = fromExpr(stmt.value)
    t0 match {
      case Type.Unit =>
      case Type.Int =>
        instructions += L0(I.Drop)
      case _ =>
        ???
    }
  }

  def fromExpr(expr: Expr): Type = {
    expr match {
      case expr: Expr.BoolOp    => fromBoolOp(expr)
      case expr: Expr.BinOp     => fromBinOp(expr)
      case expr: Expr.UnaryOp   => fromUnaryOp(expr)
      case expr: Expr.Compare   => ???
      case expr: Expr.Call      => ???
      case expr: Expr.Constant  => ???
      case expr: Expr.IntLit    => fromIntLit(expr)
      case expr: Expr.StringLit => ???
      case expr: Expr.CharLit   => fromCharLit(expr)
      case expr: Expr.Name      => ???
      case expr: Expr.Tuple     => ???
    }
  }

  def fromBoolOp(expr: Expr.BoolOp): Type = {
    val t0 = fromExpr(expr.values.head)
    expr.op match {
      case Boolop.And =>
        val l0 = createLabel("L")
        instructions ++= List(
          L0(I.Get(-1)),
          L1(I.Jz(_), l0),
          L0(I.Drop)
        )
        val t1 = fromExpr(expr.values(1))
        if (t1 != Type.Int) {
          fail("invalid types")
        }
        instructions += Label(l0)
      case Boolop.Or =>
        val l0 = createLabel("L")
        instructions ++= List(
          L0(I.Get(-1)),
          L1(I.Jnz(_), l0),
          L0(I.Drop)
        )
        val t1 = fromExpr(expr.values(1))
        if (t1 != Type.Int) {
          fail("invalid types")
        }
        instructions += Label(l0)
    }
    Type.Int
  }

  def fromBinOp(expr: Expr.BinOp): Type = {
    val t0 = fromExpr(expr.left)
    val t1 = fromExpr(expr.right)
    if ((t0, t1) != (Type.Int, Type.Int)) {
      fail("invalid types")
    }
    expr.op match {
      case Operator.Add      => instructions += L0(I.Add)
      case Operator.Sub      => instructions += L0(I.Sub)
      case Operator.Mult     => instructions += L0(I.Mul)
      case Operator.MatMult  => ???
      case Operator.Div      => ???
      case Operator.Mod      => ???
      case Operator.Pow      => ???
      case Operator.LShift   => instructions += L0(I.Shl)
      case Operator.RShift   => instructions += L0(I.ShrS)
      case Operator.BitOr    => instructions += L0(I.Or)
      case Operator.BitXor   => instructions += L0(I.Xor)
      case Operator.BitAnd   => instructions += L0(I.And)
      case Operator.FloorDiv => ???
    }
    Type.Int
  }

  def fromUnaryOp(expr: Expr.UnaryOp): Type = {
    expr.op match {
      case Unaryop.Invert =>
        val t0 = fromExpr(expr.operand)
        if (t0 != Type.Int) {
          fail("invalid types")
        }
        instructions += L0(I.Not)
      case Unaryop.Not =>
        val t0 = fromExpr(expr.operand)
        if (t0 != Type.Int) {
          fail("invalid types")
        }
        instructions += L0(I.Eqz)
      case Unaryop.UAdd =>
        val t0 = fromExpr(expr.operand)
        if (t0 != Type.Int) {
          fail("invalid types")
        }
      case Unaryop.USub =>
        instructions += L0(I.Const(0))
        val t0 = fromExpr(expr.operand)
        if (t0 != Type.Int) {
          fail("invalid types")
        }
        instructions += L0(I.Sub)
    }
    Type.Int
  }

  def fromIntLit(expr: Expr.IntLit): Type = {
    instructions += L0(I.Const(expr.value))
    Type.Int
  }

  def fromCharLit(expr: Expr.CharLit): Type = {
    instructions += L0(I.Const(expr.value.toInt))
    Type.Int
  }
}

object CodeGenerator {
  def generate(mod: Mod): Either[String, ArrayBuffer[Labeled[Instruction]]] = {
    val gen = new CodeGenerator
    try {
      gen.fromModule(mod)

    } catch {
      case CodeGenerationException(msg) =>
        return Left(msg)
    }
    return Right(gen.instructions)
  }
}
