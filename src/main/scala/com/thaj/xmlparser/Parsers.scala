package com.thaj.xmlparser

import zio.Chunk
import zio.parser.Parser

trait Parsers {
  def stringLiteral: Parser[String, Char, String] =
    Parser
      .charIn('"')
      .zip(Parser.charNotIn('"').repeat)
      .zip(Parser.charIn('"'))
      .map({ case (_, b, _) => b.mkString })

  def nonWS: Parser[String, Char, Chunk[Char]] =
    Parser.charNotIn(' ').repeat

  def ws: Parser[String, Char, Unit] =
    Parser.charIn(' ').repeat0.unit

}
