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

  def NAME: Parser[identifier] =
    acceptMatch("name", { case NAME(name) => identifier(name) })

  def STRING: Parser[String] =
    acceptMatch("string", { case STRING(value) => value })

  def NUMBER: Parser[Int] =
    acceptMatch("number", { case NUMBER(value) => value })

  implicit def token(s: String): Parser[Token] =
    (delimiterMap.get(s) orElse keywordMap.get(s)).get

  def file: Parser[mod] =
    statements.? <~ ENDMARKER ^^ { a =>
      mod.Module(a.toList.flatten)
    }

  def statements: Parser[Seq[stmt]] =
    statement.+ ^^ { _.flatten }

  def statement: Parser[Seq[stmt]] =
    compound_stmt ^^ { List(_) } |
    simple_stmts

  def simple_stmts: Parser[Seq[stmt]] =
    simple_stmt <~ not(";") ~ NEWLINE ^^ { List(_) } |
    rep1sep(simple_stmt, ";") <~ ";".? ~ NEWLINE

  def simple_stmt: Parser[stmt] =
    assignment |
    star_expressions ^^ { stmt.Expr(_) }

  def compound_stmt: Parser[stmt] =
    if_stmt |
    while_stmt

  def assignment: Parser[stmt] =
    (star_targets <~ "=").+ ~ star_expressions <~ not("=") ^^ { case a ~ b =>
      stmt.Assign(a, b)
    } |
    single_target ~ augassign ~ star_expressions ^^ { case a ~ b ~ c =>
      stmt.AugAssign(a, b, c)
    }

  def augassign: Parser[operator] =
    "+=" ^^^ { operator.Add } |
    "-=" ^^^ { operator.Sub } |
    "*=" ^^^ { operator.Mult } |
    "@=" ^^^ { operator.MatMult } |
    "/=" ^^^ { operator.Div } |
    "%=" ^^^ { operator.Mod } |
    "&=" ^^^ { operator.BitAnd } |
    "|=" ^^^ { operator.BitOr } |
    "^=" ^^^ { operator.BitXor } |
    "<<=" ^^^ { operator.LShift } |
    ">>=" ^^^ { operator.RShift } |
    "**=" ^^^ { operator.Pow } |
    "//=" ^^^ { operator.FloorDiv }

  def if_stmt: Parser[stmt] =
    "if" ~ named_expression ~ ":" ~ block ~ elif_stmt ^^ {
      case _ ~ test ~ _ ~ body ~ orelse =>
        stmt.If(test, body, List(orelse))
    } |
    "if" ~ named_expression ~ ":" ~ block ~ else_block.? ^^ {
      case _ ~ test ~ _ ~ body ~ orelse =>
        stmt.If(test, body, orelse.toList.flatten)
    }

  def elif_stmt: Parser[stmt] =
    "elif" ~ named_expression ~ ":" ~ block ~ elif_stmt ^^ {
      case _ ~ test ~ _ ~ body ~ orelse =>
        stmt.If(test, body, List(orelse))
    } |
    "elif" ~ named_expression ~ ":" ~ block ~ else_block.? ^^ {
      case _ ~ test ~ _ ~ body ~ orelse =>
        stmt.If(test, body, orelse.toList.flatten)
    }

  def else_block: Parser[Seq[stmt]] =
    "else" ~> ":" ~> block

  def while_stmt: Parser[stmt] =
    "while" ~ named_expression ~ ":" ~ block ^^ { case _ ~ test ~ _ ~ body =>
      stmt.While(test, body)
    }

  def block: Parser[Seq[stmt]] =
    NEWLINE ~ INDENT ~> statements <~ DEDENT |
    simple_stmts

  def star_expressions: Parser[expr] =
    star_expression

  def star_expression: Parser[expr] =
    expression

  def named_expression: Parser[expr] =
    expression

  def expression: Parser[expr] =
    disjunction

  def disjunction: Parser[expr] =
    conjunction ~ ("or" ~> conjunction).+ ^^ { a =>
      expr.BoolOp(boolop.Or, mkList(a))
    } |
    conjunction

  def conjunction: Parser[expr] =
    inversion ~ ("and" ~> inversion).+ ^^ { a =>
      expr.BoolOp(boolop.And, mkList(a))
    } |
    inversion

  def inversion: Parser[expr] =
    "not" ~> inversion ^^ { expr.UnaryOp(unaryop.Not, _) } |
    comparison

  def comparison: Parser[expr] =
    bitwise_or ~ compare_op_bitwise_or_pair.+ ^^ { case a ~ b =>
      val (ops, comparators) = b.unzip
      expr.Compare(a, ops, comparators)
    } |
    bitwise_or

  def compare_op_bitwise_or_pair: Parser[Tuple2[cmpop, expr]] =
    "==" ~> bitwise_or ^^ { (cmpop.Eq, _) } |
    "!=" ~> bitwise_or ^^ { (cmpop.NotEq, _) } |
    "<=" ~> bitwise_or ^^ { (cmpop.LtE, _) } |
    "<" ~> bitwise_or ^^ { (cmpop.Lt, _) } |
    ">=" ~> bitwise_or ^^ { (cmpop.GtE, _) } |
    ">" ~> bitwise_or ^^ { (cmpop.Gt, _) }

  def bitwise_or: Parser[expr] =
    chainl1(
      bitwise_xor,
      "|" ^^^ { expr.BinOp(_, operator.BitOr, _) }
    )

  def bitwise_xor: Parser[expr] =
    chainl1(
      bitwise_and,
      "^" ^^^ { expr.BinOp(_, operator.BitXor, _) }
    )

  def bitwise_and: Parser[expr] =
    chainl1(
      shift_expr,
      "&" ^^^ { expr.BinOp(_, operator.BitAnd, _) }
    )

  def shift_expr: Parser[expr] =
    chainl1(
      sum,
      "<<" ^^^ { expr.BinOp(_, operator.LShift, _) } |
      ">>" ^^^ { expr.BinOp(_, operator.RShift, _) }
    )

  def sum: Parser[expr] =
    chainl1(
      term,
      "+" ^^^ { expr.BinOp(_, operator.Add, _) } |
      "-" ^^^ { expr.BinOp(_, operator.Sub, _) }
    )

  def term: Parser[expr] =
    chainl1(
      factor,
      "*" ^^^ { expr.BinOp(_, operator.Mult, _) } |
      "/" ^^^ { expr.BinOp(_, operator.Div, _) } |
      "//" ^^^ { expr.BinOp(_, operator.FloorDiv, _) } |
      "%" ^^^ { expr.BinOp(_, operator.Mod, _) } |
      "@" ^^^ { expr.BinOp(_, operator.MatMult, _) }
    )

  def factor: Parser[expr] =
    "+" ~> power ^^ { expr.UnaryOp(unaryop.UAdd, _) } |
    "-" ~> power ^^ { expr.UnaryOp(unaryop.USub, _) } |
    "~" ~> power ^^ { expr.UnaryOp(unaryop.Invert, _) } |
    power

  def power: Parser[expr] =
    await_primary ~ "**" ~ factor ^^ { case a ~ _ ~ b =>
      expr.BinOp(a, operator.Pow, b)
    } |
    await_primary

  def await_primary: Parser[expr] =
    primary

  def primary: Parser[expr] =
    atom

  def atom: Parser[expr] =
    NAME ^^ { expr.Name(_, expr_context.Load) } |
    "True" ^^^ { expr.Constant(constant.True) } |
    "False" ^^^ { expr.Constant(constant.False) } |
    "None" ^^^ { expr.Constant(constant.None) } |
    strings |
    NUMBER ^^ { expr.IntLit(_) } |
    "..." ^^^ { expr.Constant(constant.Ellipsis) }

  def strings: Parser[expr] =
    STRING.+ ^^ { a => expr.StringLit(a.mkString) }

  def star_targets: Parser[expr] =
    star_target <~ not(",") |
    star_target ~ ("," ~> star_target).* <~ ",".? ^^ { a =>
      expr.Tuple(mkList(a), expr_context.Store)
    }

  def star_target: Parser[expr] =
    target_with_star_atom

  def target_with_star_atom: Parser[expr] =
    star_atom

  def star_atom: Parser[expr] =
    NAME ^^ { expr.Name(_, expr_context.Store) }

  def single_target: Parser[expr] =
    NAME ^^ { expr.Name(_, expr_context.Store) }
}

object Parser {
  case class ParseError(val msg: String, val pos: Position)

  def parse(source: String): Either[ParseError, mod] = {
    val tokens = Lexer.tokenize(source)
    val parser = new Parser()
    val result = parser.file(new TokenReader(tokens))
    result match {
      case parser.Success(result, _) => Right(result)
      case ns: parser.NoSuccess      => Left(ParseError(ns.msg, ns.next.pos))
    }
  }
}
