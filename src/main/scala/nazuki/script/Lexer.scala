package nazuki.script

/** Based on
  *   - https://docs.python.org/3/reference/lexical_analysis.html
  *   - https://github.com/python/cpython/blob/3.10/Parser/tokenizer.c
  */

import scala.collection.mutable.{ArrayBuffer, Stack}
import scala.util.control.Breaks._
import scala.util.parsing.input._

import Token._

class Lexer(source: CharSequence) {
  private var offset = 0
  private var indentStack = new Stack[String]
  private var atBeginOfLine = true

  def getTokenPos: OffsetPosition = new OffsetPosition(source, offset)

  def peekChar: Option[Char] =
    if (offset < source.length)
      Some(source.charAt(offset))
    else
      None

  def nextChar(): Unit =
    if (offset < source.length)
      offset += 1

  def skipWhitespace(): Unit = {
    breakable {
      while (true) {
        peekChar match {
          case Some(' ' | '\t') =>
            nextChar()
          case Some('#') =>
            skipWhile {
              case Some('\n') => false
              case _          => true
            }
          case _ =>
            break()
        }
      }
    }
  }

  def skipWhile(f: Option[Char] => Boolean): Unit = {
    while (true) {
      if (f(peekChar))
        nextChar()
      else
        return
    }
  }

  def nextToken(): Token = {
    // Get indentation level
    if (atBeginOfLine) {
      atBeginOfLine = false
      var tokenOffset = offset
      val indentBuilder = new StringBuilder

      breakable(while (true) peekChar match {
        case Some(space @ (' ' | '\t')) =>
          indentBuilder += space
          nextChar()

        case Some('#') =>
          nextChar()
          breakable(while (true) peekChar match {
            case Some('\n') | None => break()
            case _                 => nextChar()
          })

        case Some('\n') =>
          nextChar()
          tokenOffset = offset
          indentBuilder.clear()

        case None =>
          return ENDMARKER

        case Some(_) =>
          val indent = indentBuilder.toString
          indentStack.headOption match {
            case None =>
              if (indent == "") {
                break()
              } else {
                indentStack.push(indent)
                return INDENT
              }
            case Some(prevIndent) =>
              if (indent == prevIndent) {
                break()
              } else if (indent.startsWith(prevIndent)) {
                indentStack.push(indent)
                return INDENT
              } else if (prevIndent.startsWith(indent)) {
                indentStack.pop()
                return DEDENT
              } else {
                return ERRORTOKEN
              }
          }
      })
    }

    breakable(while (true) peekChar match {
      // Skip spaces
      case Some(' ' | '\t') =>
        nextChar()

      // Skip a comment
      case Some('#') =>
        nextChar()
        breakable(while (true) peekChar match {
          case Some('\n') | None => break()
          case _                 => nextChar()
        })

      // Newline
      case Some('\n') =>
        nextChar()
        atBeginOfLine = true
        return NEWLINE

      // Line continuation
      case Some('\\') =>
        nextChar()
        peekChar match {
          case Some('\n') =>
            nextChar()
          case _ =>
            return ERRORTOKEN
        }

      case None =>
        atBeginOfLine = true
        return NEWLINE

      // Identifier
      case Some(c) if c.isUnicodeIdentifierStart =>
        val begin = offset
        nextChar()
        breakable(while (true) peekChar match {
          case Some(c) if c.isUnicodeIdentifierPart => nextChar()
          case _                                    => break()
        })
        val end = offset
        return source.subSequence(begin, end).toString match {
          case "False"    => FALSE
          case "None"     => NONE
          case "True"     => TRUE
          case "and"      => AND
          case "as"       => AS
          case "assert"   => ASSERT
          case "async"    => ASYNC
          case "await"    => AWAIT
          case "break"    => BREAK
          case "class"    => CLASS
          case "continue" => CONTINUE
          case "def"      => DEF
          case "del"      => DEL
          case "elif"     => ELIF
          case "else"     => ELSE
          case "except"   => EXCEPT
          case "finally"  => FINALLY
          case "for"      => FOR
          case "from"     => FROM
          case "global"   => GLOBAL
          case "if"       => IF
          case "import"   => IMPORT
          case "in"       => IN
          case "is"       => IS
          case "lambda"   => LAMBDA
          case "nonlocal" => NONLOCAL
          case "not"      => NOT
          case "or"       => OR
          case "pass"     => PASS
          case "raise"    => RAISE
          case "return"   => RETURN
          case "try"      => TRY
          case "while"    => WHILE
          case "with"     => WITH
          case "yield"    => YIELD
          case name       => NAME(name)
        }

      case Some(c) if '0' <= c && c <= '9' =>
        val begin = offset
        nextChar()
        breakable(while (true) peekChar match {
          case Some(c) if '0' <= c && c <= '9' => nextChar()
          case _                               => break()
        })
        val end = offset
        return NUMBER(source.subSequence(begin, end).toString.toInt)

      case Some('(') =>
        nextChar()
        return LPAR

      case Some(')') =>
        nextChar()
        return RPAR

      case Some('[') =>
        nextChar()
        return LSQB

      case Some(']') =>
        nextChar()
        return RSQB

      case Some(':') =>
        nextChar()
        return COLON

      case Some(',') =>
        nextChar()
        return COMMA

      case Some(';') =>
        nextChar()
        return SEMI

      case Some('+') =>
        nextChar()
        peekChar match {
          case Some('=') =>
            nextChar()
            return PLUSEQUAL
          case _ =>
            return PLUS
        }

      case Some('-') =>
        nextChar()
        peekChar match {
          case Some('=') =>
            nextChar()
            return MINEQUAL
          case Some('>') =>
            nextChar()
            return RARROW
          case _ =>
            return MINUS
        }

      case Some('*') =>
        nextChar()
        peekChar match {
          case Some('*') =>
            nextChar()
            peekChar match {
              case Some('=') =>
                nextChar()
                return DOUBLESTAREQUAL
              case _ =>
                return DOUBLESTAR
            }
          case Some('=') =>
            nextChar()
            return STAREQUAL
          case _ =>
            return STAR
        }

      case Some('/') =>
        nextChar()
        peekChar match {
          case Some('/') =>
            nextChar()
            peekChar match {
              case Some('=') =>
                nextChar()
                return DOUBLESLASHEQUAL
              case _ =>
                return DOUBLESLASH
            }
          case Some('=') =>
            nextChar()
            return SLASHEQUAL
          case _ =>
            return SLASH
        }

      case Some('|') =>
        nextChar()
        peekChar match {
          case Some('=') =>
            nextChar()
            return VBAREQUAL
          case _ =>
            return VBAR
        }

      case Some('&') =>
        nextChar()
        peekChar match {
          case Some('=') =>
            nextChar()
            return AMPEREQUAL
          case _ =>
            return AMPER
        }

      case Some('<') =>
        nextChar()
        peekChar match {
          case Some('=') =>
            nextChar()
            return LESSEQUAL
          case Some('<') =>
            nextChar()
            peekChar match {
              case Some('=') =>
                nextChar()
                return LEFTSHIFTEQUAL
              case _ =>
                return LEFTSHIFT
            }
          case _ =>
            return LESS
        }

      case Some('>') =>
        nextChar()
        peekChar match {
          case Some('=') =>
            nextChar()
            return GREATEREQUAL
          case Some('>') =>
            nextChar()
            peekChar match {
              case Some('=') =>
                nextChar()
                return RIGHTSHIFTEQUAL
              case _ =>
                return RIGHTSHIFT
            }
          case _ =>
            return GREATER
        }

      case Some('=') =>
        nextChar()
        peekChar match {
          case Some('=') =>
            nextChar()
            return EQEQUAL
          case _ =>
            return EQUAL
        }

      case Some('.') =>
        nextChar()
        peekChar match {
          case Some('.') =>
            nextChar()
            peekChar match {
              case Some('.') =>
                nextChar()
                return ELLIPSIS
              case _ =>
                return ERRORTOKEN
            }
          case _ =>
            return DOT
        }

      case Some('%') =>
        nextChar()
        peekChar match {
          case Some('=') =>
            nextChar()
            return PERCENTEQUAL
          case _ =>
            return PERCENT
        }

      case Some('{') =>
        nextChar()
        return LBRACE

      case Some('}') =>
        nextChar()
        return RBRACE

      case Some('!') =>
        nextChar()
        peekChar match {
          case Some('=') =>
            nextChar()
            return NOTEQUAL
          case _ =>
            return ERRORTOKEN
        }

      case Some('~') =>
        nextChar()
        return TILDE

      case Some('^') =>
        nextChar()
        peekChar match {
          case Some('=') =>
            nextChar()
            return CIRCUMFLEXEQUAL
          case _ =>
            return CIRCUMFLEX
        }

      case Some('@') =>
        nextChar()
        peekChar match {
          case Some('=') =>
            nextChar()
            return ATEQUAL
          case _ =>
            return AT
        }

      case Some(_) =>
        return ERRORTOKEN
    })

    ERRORTOKEN
  }
}

object Lexer {
  def tokenize(source: String): Seq[Tuple2[Token, Position]] = {
    val lexer = new Lexer(source)
    val tokens = new ArrayBuffer[Tuple2[Token, Position]]
    breakable(while (true) lexer.nextToken() match {
      case token @ ENDMARKER =>
        tokens.append((token, NoPosition))
        break()
      case token @ ERRORTOKEN =>
        tokens.append((token, NoPosition))
        break()
      case token =>
        tokens.append((token, NoPosition))
    })
    tokens.toList
  }
}
