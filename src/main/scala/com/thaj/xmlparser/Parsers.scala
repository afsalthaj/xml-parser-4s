package com.thaj.xmlparser

import com.thaj.xmlparser.XmlObject.Text
import zio.Chunk
import zio.parser.Parser

trait Parsers {
  def stringLiteral: Parser[String, Char, String] =
    Parser
      .charIn('"')
      .zip(Parser.charNotIn('"').repeat)
      .zip(Parser.charIn('"'))
      .map({ case (_, b, _) => b.mkString.trim })

  def nonWS: Parser[String, Char, Chunk[Char]] =
    Parser.charNotIn(' ', '\n').repeat

  def ws: Parser[String, Char, Unit] =
    Parser.charIn(' ', '\n').repeat0.unit

  lazy val tagIdentifier =
    Parser.charNotIn('<', '>', ' ').repeat.map(_.mkString.trim)

  def textParser =
    Parser.charNotIn('<', '>', '=').repeat.map(s => Text(s.mkString.trim))

  lazy val attrKeyParser: Parser[String, Char, String] =
    Parser
      .charNotIn(' ', '=')
      .repeat
      .map(_.mkString.trim)

  lazy val attributeValueParser: Parser[String, Char, String] =
    stringLiteral

  lazy val attributeParser: Parser[String, Char, (String, String)] =
    attrKeyParser
      .zip(ws)
      .zip(Parser.charIn('=').unit)
      .zip(ws)
      .zip(attributeValueParser)

  lazy val openAngular: Parser[String, Char, Unit] =
    Parser.charIn('<').unit

  lazy val closedAngular: Parser[String, Char, Unit] =
    Parser.charIn('>').unit

  lazy val closedTag: Parser[String, Char, (Char, Char, String)] =
    Parser
      .charIn('<')
      .zip(Parser.charIn('/'))
      .zip(ws)
      .zip(tagIdentifier)
      .zip(ws)
      .zip(closedAngular)

}
