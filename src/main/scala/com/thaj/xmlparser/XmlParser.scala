package com.thaj.xmlparser

import com.thaj.xmlparser.XmlObject.TagElement
import com.thaj.xmlparser.XmlParser.{attributeParser, xmlParser}
import zio.Chunk
import zio.parser.Parser

object XmlParser extends Parsers {
  lazy val tagContent: Parser[String, Char, Option[Chunk[XmlObject]]] =
    xmlParser.repeat
      .orElseEither(textParser.zip(ws).map(Chunk(_)))
      .map(_.merge)
      .optional

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
            Right(
              TagElement(
                name,
                attributes,
                Chunk.fromIterable(xmlObject).flatten
              )
            )
          } else {
            Left(s"Closed tag ${closedTag} is not the same as open tag ${name}")
          }
      })
  }

  def parse(string: String): Either[Parser.ParserError[String], XmlObject] =
    xmlParser.parseString(string)

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
       |   <sub1>
       |     text1
       |   </sub1>
       |   <sub2 a = "b">
       |      text2
       |   </sub2>
       |   <sub3 a = "c">
       |     text3
       |   </sub3>
       | </simpletag>
       |
       |
       |
       |""".stripMargin

  println(xmlParser.parseString(nextTest))

  val html =
    s"""
       |<body >
       | <p style =    "color:red"> test
       | </p>
       |</body>
       |""".stripMargin

  println(xmlParser.parseString(html))

}
