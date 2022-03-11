package nazuki.script.syntax

/** Based on
  *   - https://docs.python.org/3/reference/grammar.html
  *   - https://github.com/python/cpython/blob/3.10/Grammar/python.gram
  */

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.language.implicitConversions

import ast._
import Token._

class Parser extends PackratParsers {
  type Elem = Token

  def NAME: Parser[Identifier] =
    acceptMatch("name", { case NAME(name) => Identifier(name) })

  def STRING: Parser[String] =
    acceptMatch("string", { case STRING(value) => value })

  def NUMBER: Parser[Int] =
    acceptMatch("number", { case NUMBER(value) => value })

  implicit def token(s: String): Parser[Token] =
    (delimiterMap.get(s) orElse keywordMap.get(s)).get

  def file: Parser[Mod] =
    statements.? <~ ENDMARKER ^^ { a =>
      Mod.Module(a.toList.flatten)
    }

  def statements: Parser[Seq[Stmt]] =
    statement.+ ^^ { _.flatten }

  def statement: Parser[Seq[Stmt]] =
    compound_stmt ^^ { List(_) } |
    simple_stmts

  def simple_stmts: Parser[Seq[Stmt]] =
    simple_stmt <~ not(";") ~ NEWLINE ^^ { List(_) } |
    rep1sep(simple_stmt, ";") <~ ";".? ~ NEWLINE

  def simple_stmt: Parser[Stmt] =
    assignment |
    star_expressions ^^ { Stmt.Expr(_) }

  def compound_stmt: Parser[Stmt] =
    if_stmt |
    while_stmt

  def assignment: Parser[Stmt] =
    (star_targets <~ "=").+ ~ star_expressions <~ not("=") ^^ { case a ~ b =>
      Stmt.Assign(a, b)
    } |
    single_target ~ augassign ~ star_expressions ^^ { case a ~ b ~ c =>
      Stmt.AugAssign(a, b, c)
    }

  def augassign: Parser[Operator] =
    "+=" ^^^ { Operator.Add } |
    "-=" ^^^ { Operator.Sub } |
    "*=" ^^^ { Operator.Mult } |
    "@=" ^^^ { Operator.MatMult } |
    "/=" ^^^ { Operator.Div } |
    "%=" ^^^ { Operator.Mod } |
    "&=" ^^^ { Operator.BitAnd } |
    "|=" ^^^ { Operator.BitOr } |
    "^=" ^^^ { Operator.BitXor } |
    "<<=" ^^^ { Operator.LShift } |
    ">>=" ^^^ { Operator.RShift } |
    "**=" ^^^ { Operator.Pow } |
    "//=" ^^^ { Operator.FloorDiv }

  def if_stmt: Parser[Stmt] =
    "if" ~ named_expression ~ ":" ~ block ~ elif_stmt ^^ {
      case _ ~ test ~ _ ~ body ~ orelse =>
        Stmt.If(test, body, List(orelse))
    } |
    "if" ~ named_expression ~ ":" ~ block ~ else_block.? ^^ {
      case _ ~ test ~ _ ~ body ~ orelse =>
        Stmt.If(test, body, orelse.toList.flatten)
    }

  def elif_stmt: Parser[Stmt] =
    "elif" ~ named_expression ~ ":" ~ block ~ elif_stmt ^^ {
      case _ ~ test ~ _ ~ body ~ orelse =>
        Stmt.If(test, body, List(orelse))
    } |
    "elif" ~ named_expression ~ ":" ~ block ~ else_block.? ^^ {
      case _ ~ test ~ _ ~ body ~ orelse =>
        Stmt.If(test, body, orelse.toList.flatten)
    }

  def else_block: Parser[Seq[Stmt]] =
    "else" ~> ":" ~> block

  def while_stmt: Parser[Stmt] =
    "while" ~ named_expression ~ ":" ~ block ^^ { case _ ~ test ~ _ ~ body =>
      Stmt.While(test, body)
    }

  def block: Parser[Seq[Stmt]] =
    NEWLINE ~ INDENT ~> statements <~ DEDENT |
    simple_stmts

  def star_expressions: Parser[Expr] =
    star_expression

  def star_expression: Parser[Expr] =
    expression

  def named_expression: Parser[Expr] =
    expression

  def expression: Parser[Expr] =
    disjunction

  def disjunction: Parser[Expr] =
    conjunction ~ ("or" ~> conjunction).+ ^^ { a =>
      Expr.BoolOp(Boolop.Or, mkList(a))
    } |
    conjunction

  def conjunction: Parser[Expr] =
    inversion ~ ("and" ~> inversion).+ ^^ { a =>
      Expr.BoolOp(Boolop.And, mkList(a))
    } |
    inversion

  def inversion: Parser[Expr] =
    "not" ~> inversion ^^ { Expr.UnaryOp(Unaryop.Not, _) } |
    comparison

  def comparison: Parser[Expr] =
    bitwise_or ~ compare_op_bitwise_or_pair.+ ^^ { case a ~ b =>
      val (ops, comparators) = b.unzip
      Expr.Compare(a, ops, comparators)
    } |
    bitwise_or

  def compare_op_bitwise_or_pair: Parser[Tuple2[Cmpop, Expr]] =
    "==" ~> bitwise_or ^^ { (Cmpop.Eq, _) } |
    "!=" ~> bitwise_or ^^ { (Cmpop.NotEq, _) } |
    "<=" ~> bitwise_or ^^ { (Cmpop.LtE, _) } |
    "<" ~> bitwise_or ^^ { (Cmpop.Lt, _) } |
    ">=" ~> bitwise_or ^^ { (Cmpop.GtE, _) } |
    ">" ~> bitwise_or ^^ { (Cmpop.Gt, _) }

  def bitwise_or: Parser[Expr] =
    chainl1(
      bitwise_xor,
      "|" ^^^ { Expr.BinOp(_, Operator.BitOr, _) }
    )

  def bitwise_xor: Parser[Expr] =
    chainl1(
      bitwise_and,
      "^" ^^^ { Expr.BinOp(_, Operator.BitXor, _) }
    )

  def bitwise_and: Parser[Expr] =
    chainl1(
      shift_expr,
      "&" ^^^ { Expr.BinOp(_, Operator.BitAnd, _) }
    )

  def shift_expr: Parser[Expr] =
    chainl1(
      sum,
      "<<" ^^^ { Expr.BinOp(_, Operator.LShift, _) } |
      ">>" ^^^ { Expr.BinOp(_, Operator.RShift, _) }
    )

  def sum: Parser[Expr] =
    chainl1(
      term,
      "+" ^^^ { Expr.BinOp(_, Operator.Add, _) } |
      "-" ^^^ { Expr.BinOp(_, Operator.Sub, _) }
    )

  def term: Parser[Expr] =
    chainl1(
      factor,
      "*" ^^^ { Expr.BinOp(_, Operator.Mult, _) } |
      "/" ^^^ { Expr.BinOp(_, Operator.Div, _) } |
      "//" ^^^ { Expr.BinOp(_, Operator.FloorDiv, _) } |
      "%" ^^^ { Expr.BinOp(_, Operator.Mod, _) } |
      "@" ^^^ { Expr.BinOp(_, Operator.MatMult, _) }
    )

  def factor: Parser[Expr] =
    "+" ~> power ^^ { Expr.UnaryOp(Unaryop.UAdd, _) } |
    "-" ~> power ^^ { Expr.UnaryOp(Unaryop.USub, _) } |
    "~" ~> power ^^ { Expr.UnaryOp(Unaryop.Invert, _) } |
    power

  def power: Parser[Expr] =
    await_primary ~ "**" ~ factor ^^ { case a ~ _ ~ b =>
      Expr.BinOp(a, Operator.Pow, b)
    } |
    await_primary

  def await_primary: Parser[Expr] =
    primary

  def primary: Parser[Expr] =
    atom

  def atom: Parser[Expr] =
    NAME ^^ { Expr.Name(_, ExprContext.Load) } |
    "True" ^^^ { Expr.Constant(Constant.True) } |
    "False" ^^^ { Expr.Constant(Constant.False) } |
    "None" ^^^ { Expr.Constant(Constant.None) } |
    strings |
    NUMBER ^^ { Expr.IntLit(_) } |
    "..." ^^^ { Expr.Constant(Constant.Ellipsis) }

  def strings: Parser[Expr] =
    STRING.+ ^^ { a => Expr.StringLit(a.mkString) }

  def star_targets: Parser[Expr] =
    star_target <~ not(",") |
    star_target ~ ("," ~> star_target).* <~ ",".? ^^ { a =>
      Expr.Tuple(mkList(a), ExprContext.Store)
    }

  def star_target: Parser[Expr] =
    target_with_star_atom

  def target_with_star_atom: Parser[Expr] =
    star_atom

  def star_atom: Parser[Expr] =
    NAME ^^ { Expr.Name(_, ExprContext.Store) }

  def single_target: Parser[Expr] =
    NAME ^^ { Expr.Name(_, ExprContext.Store) }
}

object Parser {
  case class ParseError(val msg: String, val pos: Position)

  def parse(source: String): Either[ParseError, Mod] = {
    val tokens = Lexer.tokenize(source)
    val parser = new Parser()
    val result = parser.file(new TokenReader(tokens))
    result match {
      case parser.Success(result, _) => Right(result)
      case ns: parser.NoSuccess      => Left(ParseError(ns.msg, ns.next.pos))
    }
  }
}
