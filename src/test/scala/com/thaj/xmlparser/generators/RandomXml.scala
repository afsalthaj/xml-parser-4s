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
) { self =>

  def print(space: Space): String = {
    def go(randomXml: RandomXml): String =
      randomXml.body match {
        case Some(children) =>
          children.value match {
            case Left(whitSpacedText) => whitSpacedText.value
            case Right(randomXmls) => randomXmls.map(go).mkString(space.toString)
          }
        case None => space.toString
      }

    go(self)
  }

  def emptyChildren: RandomXml =
    copy(body = None)

  def toXmlObject: XmlObject = {

    val attributes: Chunk[XmlObject.Attribute] =
      openTag.attributes.value.map { case (randomAttributeWithSpace, _) => randomAttributeWithSpace.toAttribute }

    val children =
      body match {
        case Some(value) =>
          value.value match {
            case Left(whiteSpacedText) => Chunk(whiteSpacedText.toXmlObjectText)
            case Right(chunkOfTags) => chunkOfTags.map(_.toXmlObject)
          }
        case None => Chunk.empty
      }

    TagElement(openTag.text.value, attributes, children)

  }
}

object RandomXml {

  final case class Children(value: Either[WhiteSpacedText, Chunk[RandomXml]])

  def gen(numberOfAttributes: Int, maxNumberOfAttributes: Int) =
    for {
      open <- OpenTag.gen(numberOfAttributes, maxNumberOfAttributes)
      children <- Gen.option(WhiteSpacedText.gen.map(value => Children(Left(value))))
      closed <- ClosingTag.gen(open.text)
    } yield RandomXml(open, children, closed)
}
