package nazuki.script

/** Based on
  *   - https://docs.python.org/3/reference/lexical_analysis.html
  *   - https://github.com/python/cpython/blob/3.10/Grammar/Tokens
  */

import scala.util.parsing.input._

enum Token {
  case ENDMARKER

  case NAME(name: String)
  case NUMBER(value: Int)
  case STRING(value: String)

  case NEWLINE
  case INDENT
  case DEDENT

  case LPAR
  case RPAR
  case LSQB
  case RSQB
  case COLON
  case COMMA
  case SEMI
  case PLUS
  case MINUS
  case STAR
  case SLASH
  case VBAR
  case AMPER
  case LESS
  case GREATER
  case EQUAL
  case DOT
  case PERCENT
  case LBRACE
  case RBRACE
  case EQEQUAL
  case NOTEQUAL
  case LESSEQUAL
  case GREATEREQUAL
  case TILDE
  case CIRCUMFLEX
  case LEFTSHIFT
  case RIGHTSHIFT
  case DOUBLESTAR
  case PLUSEQUAL
  case MINEQUAL
  case STAREQUAL
  case SLASHEQUAL
  case PERCENTEQUAL
  case AMPEREQUAL
  case VBAREQUAL
  case CIRCUMFLEXEQUAL
  case LEFTSHIFTEQUAL
  case RIGHTSHIFTEQUAL
  case DOUBLESTAREQUAL
  case DOUBLESLASH
  case DOUBLESLASHEQUAL
  case AT
  case ATEQUAL
  case RARROW
  case ELLIPSIS
  case COLONEQUAL

  case ERRORTOKEN

  // Keywords
  case FALSE
  case NONE
  case TRUE
  case AND
  case AS
  case ASSERT
  case ASYNC
  case AWAIT
  case BREAK
  case CLASS
  case CONTINUE
  case DEF
  case DEL
  case ELIF
  case ELSE
  case EXCEPT
  case FINALLY
  case FOR
  case FROM
  case GLOBAL
  case IF
  case IMPORT
  case IN
  case IS
  case LAMBDA
  case NONLOCAL
  case NOT
  case OR
  case PASS
  case RAISE
  case RETURN
  case TRY
  case WHILE
  case WITH
  case YIELD
}

object Token {
  val delimiterMap = Map(
    "(" -> LPAR,
    ")" -> RPAR,
    "[" -> LSQB,
    "]" -> RSQB,
    ":" -> COLON,
    "," -> COMMA,
    ";" -> SEMI,
    "+" -> PLUS,
    "-" -> MINUS,
    "*" -> STAR,
    "/" -> SLASH,
    "|" -> VBAR,
    "&" -> AMPER,
    "<" -> LESS,
    ">" -> GREATER,
    "=" -> EQUAL,
    "." -> DOT,
    "%" -> PERCENT,
    "{" -> LBRACE,
    "}" -> RBRACE,
    "==" -> EQEQUAL,
    "!=" -> NOTEQUAL,
    "<=" -> LESSEQUAL,
    ">=" -> GREATEREQUAL,
    "~" -> TILDE,
    "^" -> CIRCUMFLEX,
    "<<" -> LEFTSHIFT,
    ">>" -> RIGHTSHIFT,
    "**" -> DOUBLESTAR,
    "+=" -> PLUSEQUAL,
    "-=" -> MINEQUAL,
    "*=" -> STAREQUAL,
    "/=" -> SLASHEQUAL,
    "%=" -> PERCENTEQUAL,
    "&=" -> AMPEREQUAL,
    "|=" -> VBAREQUAL,
    "^=" -> CIRCUMFLEXEQUAL,
    "<<=" -> LEFTSHIFTEQUAL,
    ">>=" -> RIGHTSHIFTEQUAL,
    "**=" -> DOUBLESTAREQUAL,
    "//" -> DOUBLESLASH,
    "//=" -> DOUBLESLASHEQUAL,
    "@" -> AT,
    "@=" -> ATEQUAL,
    "->" -> RARROW,
    "..." -> ELLIPSIS,
    ":=" -> COLONEQUAL
  )

  val keywordMap = Map(
    "False" -> FALSE,
    "None" -> NONE,
    "True" -> TRUE,
    "and" -> AND,
    "as" -> AS,
    "assert" -> ASSERT,
    "async" -> ASYNC,
    "await" -> AWAIT,
    "break" -> BREAK,
    "class" -> CLASS,
    "continue" -> CONTINUE,
    "def" -> DEF,
    "del" -> DEL,
    "elif" -> ELIF,
    "else" -> ELSE,
    "except" -> EXCEPT,
    "finally" -> FINALLY,
    "for" -> FOR,
    "from" -> FROM,
    "global" -> GLOBAL,
    "if" -> IF,
    "import" -> IMPORT,
    "in" -> IN,
    "is" -> IS,
    "lambda" -> LAMBDA,
    "nonlocal" -> NONLOCAL,
    "not" -> NOT,
    "or" -> OR,
    "pass" -> PASS,
    "raise" -> RAISE,
    "return" -> RETURN,
    "try" -> TRY,
    "while" -> WHILE,
    "with" -> WITH,
    "yield" -> YIELD
  )
}
