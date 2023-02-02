package com.thaj.xmlparser.generators

import com.thaj.xmlparser.XmlObject
import com.thaj.xmlparser.XmlObject.TagElement
import com.thaj.xmlparser.generators.RandomXml.Children
import zio.Chunk
import zio.test.Gen

final case class RandomXml(
  openTag: OpenTag,
  body: Option[Children],
  closingTag: ClosingTag
) {

  def emptyChildren: RandomXml =
    copy(body = None)

  def toXmlObject: XmlObject = {

    val attributes: Chunk[XmlObject.Attribute] =
      openTag.attributes.value.map { case (randomAttributeWithSpace, _) => randomAttributeWithSpace.toAttribute }

    val children =
      body match {
        case Some(value) =>
          value.body match {
            case Left(whiteSpacedText) => Chunk(whiteSpacedText.toXmlObjectText)
            case Right(chunkOfTags) => chunkOfTags.map(_.toXmlObject)
          }
        case None => Chunk.empty
      }

    TagElement(openTag.text.value, attributes, children)

  }
}

object RandomXml {

  final case class Children(body: Either[WhiteSpacedText, Chunk[RandomXml]])

  def gen(numberOfAttributes: Int, maxNumberOfAttributes: Int) =
    for {
      open <- OpenTag.gen(numberOfAttributes, maxNumberOfAttributes)
      children <- Gen.option(WhiteSpacedText.gen.map(value => Children(Left(value))))
      closed <- ClosingTag.gen(open.text)
    } yield RandomXml(open, children, closed)
}
