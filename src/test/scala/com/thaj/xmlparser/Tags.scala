package com.thaj.xmlparser

import com.thaj.xmlparser.XmlObject.Attribute
import zio.Chunk
import zio.test.Gen

final case class Tags(
    openTag: OpenTag,
    closingTag: ClosingTag
)

object Tags {
  val gen: Gen[Any, Tags] =
    for {
      open <- OpenTag.gen
      closed <- ClosingTag.gen(open.text)
    } yield Tags(open, closed)
}
