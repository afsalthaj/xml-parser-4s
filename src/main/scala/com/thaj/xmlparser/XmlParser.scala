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
  ] =
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
      .transformEither { case (name, attributes, xmlObject, (_, _, closedTag)) =>
        if (name == closedTag) {
          Right(
            TagElement(
              name,
              attributes,
              Chunk.fromIterable(xmlObject).flatten
            )
          )
        } else {
          Left(s"Closed tag $closedTag is not the same as open tag $name")
        }
      }

  def parse(string: String): Either[Parser.ParserError[String], XmlObject] =
    xmlParser.parseString(string)

}

object TestApp extends App {

//  println(
//    attributeParser
//      .parseString("key=\"value\"")
//  )
//
//  val str =
//    s"""
//       |
//       |<head key=\"value\" key=\"value2\"> hello </head>
//       |
//       |""".stripMargin
//
//  println(
//    xmlParser
//      .parseString(str)
//  )
//
//  val nextTest =
//    s"""
//       | <simpletag a = "a">
//       |   <sub1>
//       |     text1
//       |   </sub1>
//       |   <sub2 a = "b">
//       |      text2
//       |   </sub2>
//       |   <sub3 a = "c">
//       |     text3
//       |   </sub3>
//       | </simpletag>
//       |
//       |
//       |
//       |""".stripMargin
//
//  println(xmlParser.parseString(nextTest).map(_.flattened))

//  Right(
//    HashMap(
//      Chunk(KeyName(simpletag), KeyName(sub2)) -> text2,
//      Chunk(KeyName(simpletag), KeyName(a)) -> a,
//      Chunk(KeyName(simpletag), KeyName(sub2), KeyName(a)) -> b,
//      Chunk(KeyName(simpletag), KeyName(sub3), KeyName(a)) -> c,
//      Chunk(KeyName(simpletag), KeyName(sub1)) -> text1,
//      Chunk(KeyName(simpletag), KeyName(sub3)) -> text3
//    )
//  )

//  val html =
//    s"""
//       |<aa\\<aa
//       |aaa
//       |="aaa"
//       |
//       |
//       |></aa\\<aa
//       |>
//       |
//       |""".stripMargin
//
  val html_ = {

  s"""
     |\t\n\r
     |<\t\n\rbody\t\n\r>
     |  <\t\n\rconfig\t\n\r>
     |    <\t\n\rkey1\t\n\r>value1</key1\t\n\r>
     |    <\t\n\rkey2\t\n\r>value2</key2\t\n\r>
     |  </config\t\n\r>
     |  \t\n\r
     |</body\t\n\r>
     |\t\n\r
     |
     |""".stripMargin


  }

  println(html_)
  println(xmlParser.parseString(html_))

  println(Parser.charNotIn('"').repeat0.parseString("\rsghbn"))

}
