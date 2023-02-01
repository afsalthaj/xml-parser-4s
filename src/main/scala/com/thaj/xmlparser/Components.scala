package com.thaj.xmlparser

import com.thaj.xmlparser.XmlObject.{TagElement, Text}
import zio.parser.Parser

object Components extends App with Parsers {
  lazy val tagIdentifier =
    Parser.charNotIn('<', '>', ' ').repeat.map(_.mkString)

  def textParser =
    Parser.charNotIn('<', '>', ' ', '=').repeat.map(s => Text(s.mkString))

  lazy val attrKeyParser: Parser[String, Char, String] =
    Parser
      .charNotIn(' ', '=')
      .repeat
      .map(_.mkString)

  lazy val attributeValueParser =
    stringLiteral

  lazy val attributeParser =
    attrKeyParser
      .zip(ws)
      .zip(Parser.charIn('=').unit)
      .zip(ws)
      .zip(attributeValueParser)

  def openTag: Parser[String, Char, Unit] =
    Parser.charIn('<').unit

  def closedTag: Parser[String, Char, Unit] =
    Parser.charIn('>').unit

  def tagParser: Parser[String, Char, String] =
    openTag
      .zip(ws)
      .zip(tagIdentifier)
      .zip(ws)
      .zip(closedTag)
      .map(_.mkString)

  lazy val tagParser_ : Parser[
    String,
    Char,
    XmlObject
  ] = {
    ws.zip(openTag)
      .zip(ws)
      .zip(tagIdentifier)
      .zip(ws)
      .zip(attributeParser.zip(ws).repeat)
      .zip(ws)
      .zip(closedTag)
      .zip(ws)
      .zip(tagParser_.orElseEither(textParser.zip(ws)))
      .map({ case (name, attributes, either) =>
        TagElement(name, attributes, either.merge)
      })
  }

  lazy val xmlParser = tagParser_

  println(
    attributeParser
      .parseString("key=\"value\"")
  )

  val str =
    s"""
       |
       |<head key=\"value\" key=\"value2\"> hello
       |
       |""".stripMargin

  println(
    tagParser_
      .parseString(str)
  )

}
