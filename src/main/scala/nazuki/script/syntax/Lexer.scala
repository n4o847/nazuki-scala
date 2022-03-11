package nazuki.script.syntax

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

  def takeChars(n: Int): Option[String] =
    if (0 <= n && offset + n <= source.length)
      Some(source.subSequence(offset, offset + n).toString)
    else
      None

  def nextChar(): Unit =
    if (offset < source.length)
      offset += 1

  def skipChars(n: Int): Unit =
    if (0 <= n && offset + n <= source.length)
      offset += n
    else if (0 <= n)
      offset = source.length

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

      // EOF
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
        val ident = source.subSequence(begin, end).toString
        for {
          token <- keywordMap.get(ident)
        } {
          return token
        }
        return NAME(ident)

      // Number
      case Some(c) if '0' <= c && c <= '9' =>
        val begin = offset
        nextChar()
        breakable(while (true) peekChar match {
          case Some(c) if '0' <= c && c <= '9' => nextChar()
          case _                               => break()
        })
        val end = offset
        return NUMBER(source.subSequence(begin, end).toString.toInt)

      // Punctuation
      case Some(_) =>
        for {
          n <- List(3, 2, 1)
          delimiter <- takeChars(n)
          token <- delimiterMap.get(delimiter)
        } {
          skipChars(n)
          return token
        }
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
