package com.thaj.xmlparser

import zio.test._
import zio.test.Assertion._
import zio.test._

object AdditionSpec extends ZIOSpecDefault {

  def spec =
    suite("Xml parser spec")(
      test("test simple with zero attributes") {
        check(
          Tags.gen(0, 0).noShrink,
          Space.gen.noShrink
        ) { (tags, space) =>
          val config =
            s"${tags.openTag}$space${tags.closingTag}"

          val parsed = XmlParser.parse(config)

          assert(parsed)(isRight)
        }
      },
      test("test simple with attributes") {
        check(
          Tags.gen(1, 10).noShrink,
          Space.gen.noShrink
        ) { (tags, space) =>
          val config =
            s"${tags.openTag}$space${tags.closingTag}"

          val parsed = XmlParser.parse(config)

          assert(parsed)(isRight)
        }
      },
      test("test simple with or without attributes, and with or without children") {
        check(
          Tags.gen(1, 10).noShrink,
          Space.gen.noShrink
        ) { (tags, space) =>
          def print(tags: Tags): String = tags.body match {
            case Some(value) =>
              value.body match {
                case Left(value) => value.value
                case Right(value) => value.map(print).mkString(space.toString)
              }
            case None => space.toString
          }

          val config =
            s"${tags.openTag}$space${print(tags)}$space${tags.closingTag}"

          val parsed = XmlParser.parse(config)

          println(parsed)
          assert(parsed)(isRight)
        }
      }
    )
}
