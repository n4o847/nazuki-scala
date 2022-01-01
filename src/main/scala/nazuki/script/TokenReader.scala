package nazuki.script

import scala.util.parsing.input._

import Token._

class TokenReader(tokens: Seq[Tuple2[Token, Position]]) extends Reader[Token] {
  def atEnd: Boolean = tokens.isEmpty

  def first: Token =
    tokens.headOption.map(_._1).getOrElse(ENDMARKER)

  def pos: Position =
    tokens.headOption.map(_._2).getOrElse(NoPosition)

  def rest: Reader[Token] = new TokenReader(tokens.tail)
}
