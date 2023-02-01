package com.thaj.xmlparser

import com.thaj.xmlparser.XmlObject.{TagElement}
import com.thaj.xmlparser.XmlParser.{attributeParser, xmlParser}
import zio.parser.Parser

object XmlParser extends Parsers {
  lazy val tagContent: Parser[String, Char, XmlObject] =
    xmlParser.orElseEither(textParser.zip(ws)).map(_.merge)

  lazy val xmlParser: Parser[
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

}

object TestApp extends App {

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
    xmlParser
      .parseString(str)
  )

  val nextTest =
    s"""
       | <simpletag a = "a">
       |   <hello>
       |     well
       |   </hello>
       | </simpletag>
       |
       |
       |
       |""".stripMargin

  println(xmlParser.parseString(nextTest))

}
