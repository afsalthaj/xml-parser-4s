package com.thaj.xmlparser

import com.thaj.xmlparser.generators.{RandomXml, Space}
import zio.test._
import zio.test.Assertion._

object AdditionSpec extends ZIOSpecDefault {

  def spec =
    suite("Xml parser spec")(
      test("test xml with zero children and with zero attributes") {
        check(
          RandomXml.gen(0, 0).noShrink,
          Space.gen.noShrink
        ) { (randomXml, space) =>
          val config =
            s"${randomXml.openTag}$space${randomXml.closingTag}"

          val parsed = XmlParser.parse(config)

          assert(parsed)(equalTo(Right(randomXml.emptyChildren.toXmlObject)))
        }
      },
      test("test xml with attributes with no children") {
        check(
          RandomXml.gen(1, 10).noShrink,
          Space.gen.noShrink
        ) { (randomXml, space) =>
          val config =
            s"${randomXml.openTag}$space${randomXml.closingTag}"

          val parsed = XmlParser.parse(config)

          assert(parsed)(equalTo(Right(randomXml.emptyChildren.toXmlObject)))
        }
      },
      // The round trip test that test any XML!
      test("test any xml with or without attributes, with or without children") {
        check(
          RandomXml.gen(1, 10).noShrink,
          Space.gen.noShrink
        ) { (randomXml, space) =>
          val config =
            s"${randomXml.openTag}$space${randomXml.print(space)}$space${randomXml.closingTag}"

          val parsed = XmlParser.parse(config)
          val expected = randomXml.toXmlObject

          assert(parsed)(equalTo(Right(expected)))
        }
      }
    )
}
