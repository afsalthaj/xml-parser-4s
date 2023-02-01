package com.thaj.xmlparser

import zio.test._
import zio.test.Assertion._
import zio.test._

object AdditionSpec extends ZIOSpecDefault {
  def spec =
    suite("Xml parser spec")(
      test("test most simplest xml") {
        check(
          Tags.gen.noShrink,
          Space.gen.noShrink
        ) { (tags, space) =>
          val config =
            s"${tags.openTag}${space}${tags.closingTag}"

          val parsed = XmlParser.parse(config)

          assert(parsed)(isRight)
        }
      }
    )
}
