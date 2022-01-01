package nazuki.script

/** Based on
  *   - https://docs.python.org/3/reference/lexical_analysis.html
  *   - https://github.com/python/cpython/blob/3.10/Grammar/Tokens
  */

import scala.util.parsing.input._

sealed trait Token

object Token {
  case object ENDMARKER extends Token

  case class NAME(name: String) extends Token
  case class NUMBER(value: Int) extends Token
  case class STRING(value: String) extends Token

  case object NEWLINE extends Token
  case object INDENT extends Token
  case object DEDENT extends Token

  case object LPAR extends Token
  case object RPAR extends Token
  case object LSQB extends Token
  case object RSQB extends Token
  case object COLON extends Token
  case object COMMA extends Token
  case object SEMI extends Token
  case object PLUS extends Token
  case object MINUS extends Token
  case object STAR extends Token
  case object SLASH extends Token
  case object VBAR extends Token
  case object AMPER extends Token
  case object LESS extends Token
  case object GREATER extends Token
  case object EQUAL extends Token
  case object DOT extends Token
  case object PERCENT extends Token
  case object LBRACE extends Token
  case object RBRACE extends Token
  case object EQEQUAL extends Token
  case object NOTEQUAL extends Token
  case object LESSEQUAL extends Token
  case object GREATEREQUAL extends Token
  case object TILDE extends Token
  case object CIRCUMFLEX extends Token
  case object LEFTSHIFT extends Token
  case object RIGHTSHIFT extends Token
  case object DOUBLESTAR extends Token
  case object PLUSEQUAL extends Token
  case object MINEQUAL extends Token
  case object STAREQUAL extends Token
  case object SLASHEQUAL extends Token
  case object PERCENTEQUAL extends Token
  case object AMPEREQUAL extends Token
  case object VBAREQUAL extends Token
  case object CIRCUMFLEXEQUAL extends Token
  case object LEFTSHIFTEQUAL extends Token
  case object RIGHTSHIFTEQUAL extends Token
  case object DOUBLESTAREQUAL extends Token
  case object DOUBLESLASH extends Token
  case object DOUBLESLASHEQUAL extends Token
  case object AT extends Token
  case object ATEQUAL extends Token
  case object RARROW extends Token
  case object ELLIPSIS extends Token
  case object COLONEQUAL extends Token

  case object ERRORTOKEN extends Token

  // Keywords
  case object FALSE extends Token
  case object NONE extends Token
  case object TRUE extends Token
  case object AND extends Token
  case object AS extends Token
  case object ASSERT extends Token
  case object ASYNC extends Token
  case object AWAIT extends Token
  case object BREAK extends Token
  case object CLASS extends Token
  case object CONTINUE extends Token
  case object DEF extends Token
  case object DEL extends Token
  case object ELIF extends Token
  case object ELSE extends Token
  case object EXCEPT extends Token
  case object FINALLY extends Token
  case object FOR extends Token
  case object FROM extends Token
  case object GLOBAL extends Token
  case object IF extends Token
  case object IMPORT extends Token
  case object IN extends Token
  case object IS extends Token
  case object LAMBDA extends Token
  case object NONLOCAL extends Token
  case object NOT extends Token
  case object OR extends Token
  case object PASS extends Token
  case object RAISE extends Token
  case object RETURN extends Token
  case object TRY extends Token
  case object WHILE extends Token
  case object WITH extends Token
  case object YIELD extends Token

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
