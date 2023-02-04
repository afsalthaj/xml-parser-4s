package com.thaj.xmlparser

import com.thaj.xmlparser.XmlObject.Text
import zio.Chunk
import zio.parser.Parser

trait Parsers {

  lazy val UnicodeEmptyCharacters: Chunk[Char] =
    Chunk(
      '\u0009', '\u000A', '\u000B', '\u000C', '\u000D', '\u0085', '\u2009', '\u200A', '\u200B', '\u205F', '\u200A',
      '\u0020', '\u00A0', '\u2000', '\u2001', '\u2002', '\u2003', '\u2004', '\u2005', '\u2006', '\u2007', '\u2008',
      '\u2009', '\u200A', '\u2028', '\u2029', '\u202F', '\u205F', '\u3000', '\u000D'
    )

  lazy val stringLiteral: Parser[String, Char, String] =
    Parser
      .charIn('"')
      .zip(Parser.charNotIn('"').repeat)
      .zip(Parser.charIn('"'))
      .map { case (_, b, _) => b.mkString }

  lazy val nonWS: Parser[String, Char, Chunk[Char]] =
    Parser.charNotIn(UnicodeEmptyCharacters: _*).repeat

  lazy val ws: Parser[String, Char, Unit] =
    Parser.charIn(UnicodeEmptyCharacters: _*).repeat0.unit

  lazy val tagIdentifier: Parser[String, Char, String] = {
    val invalid =
      Chunk('<', '>') ++ UnicodeEmptyCharacters

    Parser.charNotIn(invalid: _*).repeat.map(_.mkString.trim)
  }

  lazy val textParser: Parser[String, Char, Text] = {
    val invalid =
      Chunk('<', '>', '=')

    Parser.charNotIn(invalid: _*).repeat.map(s => Text(s.mkString.trim))
  }

  lazy val attrKeyParser: Parser[String, Char, String] = {
    val invalid =
      Chunk('=') ++ UnicodeEmptyCharacters
    Parser
      .charNotIn(invalid: _*)
      .repeat
      .map(v => v.mkString.trim)
  }

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
