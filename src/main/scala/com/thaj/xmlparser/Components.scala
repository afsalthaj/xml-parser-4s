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

  def openAngular: Parser[String, Char, Unit] =
    Parser.charIn('<').unit

  def closedAngular: Parser[String, Char, Unit] =
    Parser.charIn('>').unit

  def closedTag =
    Parser
      .charIn('<')
      .zip(Parser.charIn('/'))
      .zip(ws)
      .zip(tagIdentifier)
      .zip(ws)
      .zip(closedAngular)

  lazy val tagContent =
    tagParser_.orElseEither(textParser.zip(ws)).map(_.merge)

  lazy val tagParser_ : Parser[
    String,
    Char,
    XmlObject
  ] = {
    ws.zip(openAngular)
      .zip(ws)
      .zip(tagIdentifier)
      .zip(ws)
      .zip(attributeParser.zip(ws).repeat0)
      .zip(ws)
      .zip(closedAngular)
      .zip(ws)
      .zip(tagContent)
      .zip(ws)
      .zip(closedTag)
      .zip(ws)
      .transformEither({
        case (name, attributes, xmlObject, (_, _, closedTag)) =>
          if (name == closedTag) {
            Right(TagElement(name, attributes, xmlObject))
          } else {
            Left(s"Closed tag ${closedTag} is not the same as open tag ${name}")
          }
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
       |<head key=\"value\" key=\"value2\"> hello </head>
       |
       |""".stripMargin

  println(
    tagParser_
      .parseString(str)
  )

}
