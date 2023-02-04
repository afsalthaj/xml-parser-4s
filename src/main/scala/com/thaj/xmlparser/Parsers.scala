package com.thaj.xmlparser

import com.thaj.xmlparser.XmlObject.Text
import zio.Chunk
import zio.parser.Parser

trait Parsers {

  lazy private[xmlparser] val UnicodeEmptyCharacters: Chunk[Char] =
    Chunk(
      '\t',
      '\r',
      ' ',
      '\n'
    )

  lazy private[xmlparser] val stringLiteral: Parser[String, Char, String] =
    Parser
      .charIn('"')
      .zip(Parser.charNotIn('"').repeat)
      .zip(Parser.charIn('"'))
      .map { case (_, b, _) => b.mkString }

  lazy private[xmlparser] val nonWS: Parser[String, Char, Chunk[Char]] =
    Parser.charNotIn(UnicodeEmptyCharacters: _*).repeat

  lazy private[xmlparser] val ws: Parser[String, Char, Unit] =
    Parser.charIn(UnicodeEmptyCharacters: _*).repeat0.unit

  lazy private[xmlparser] val tagIdentifier: Parser[String, Char, String] = {
    val invalid =
      Chunk('<', '>') ++ UnicodeEmptyCharacters

    Parser.charNotIn(invalid: _*).repeat.map(_.mkString.trim)
  }

  lazy private[xmlparser] val textParser: Parser[String, Char, Text] = {
    val invalid =
      Chunk('<', '>', '=')

    Parser.charNotIn(invalid: _*).repeat.map(s => Text(s.mkString.trim))
  }

  lazy private[xmlparser] val attrKeyParser: Parser[String, Char, String] = {
    val invalid =
      Chunk('=') ++ UnicodeEmptyCharacters
    Parser
      .charNotIn(invalid: _*)
      .repeat
      .map(v => v.mkString.trim)
  }

  lazy private[xmlparser] val attributeValueParser: Parser[String, Char, String] =
    stringLiteral

  lazy private[xmlparser] val attributeParser: Parser[String, Char, (String, String)] =
    attrKeyParser
      .zip(ws)
      .zip(Parser.charIn('=').unit)
      .zip(ws)
      .zip(attributeValueParser)

  lazy private[xmlparser] val openAngular: Parser[String, Char, Unit] =
    Parser.charIn('<').unit

  lazy private[xmlparser] val closedAngular: Parser[String, Char, Unit] =
    Parser.charIn('>').unit

  lazy private[xmlparser] val closedTag: Parser[String, Char, (Char, Char, String)] =
    Parser
      .charIn('<')
      .zip(Parser.charIn('/'))
      .zip(ws)
      .zip(tagIdentifier)
      .zip(ws)
      .zip(closedAngular)

}
