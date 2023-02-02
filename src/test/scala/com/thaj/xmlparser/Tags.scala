package com.thaj.xmlparser

import com.thaj.xmlparser.Tags.Children
import zio.Chunk
import zio.test.Gen

final case class Tags(
  openTag: OpenTag,
  body: Option[Children],
  closingTag: ClosingTag
)

object Tags {

  final case class Children(body: Either[WhiteSpacedText, Chunk[Tags]])

  def gen(numberOfAttributes: Int, maxNumberOfAttributes: Int) =
    for {
      open <- OpenTag.gen(numberOfAttributes, maxNumberOfAttributes)
      children <- Gen.option(WhiteSpacedText.gen.map(value => Children(Left(value))))
      closed <- ClosingTag.gen(open.text)
    } yield Tags(open, children, closed)
}
