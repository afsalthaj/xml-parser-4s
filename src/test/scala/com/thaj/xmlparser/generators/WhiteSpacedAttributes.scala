package com.thaj.xmlparser.generators

import com.thaj.xmlparser.XmlObject
import com.thaj.xmlparser.generators.WhiteSpacedAttributes.RandomAttribute
import zio.Chunk
import zio.test.Gen

final case class WhiteSpacedAttributes private (value: Chunk[(RandomAttribute, Space)]) {

  def print: String =
    Printer.print(value.map { case (attribute, space) => Printer.print(attribute.print, space.print) }: _*)
}

object WhiteSpacedAttributes {

  def gen(minNumberOfAttributes: Int, maxNumberOfAttributes: Int): Gen[Any, WhiteSpacedAttributes] =
    for {
      numberOfAttributes <- Gen.int(minNumberOfAttributes, maxNumberOfAttributes)
      randomAttributes <- Gen.chunkOfN(numberOfAttributes)(RandomAttribute.gen)
      spaces <- Gen.chunkOfN(numberOfAttributes)(Space.gen(1))
      value = randomAttributes
        .zip(spaces)
    } yield WhiteSpacedAttributes(value)

  final case class RandomAttribute(
    key: WhiteSpacedText,
    value: WhiteSpacedText
  ) {

    def toAttribute: XmlObject.Attribute =
      (key.value, value.value)

    def print: String =
      Printer.print(key.print, "=", "\"", value.print, "\"")
  }

  object RandomAttribute {

    def gen =
      for {
        start <- Space.gen(0)
        stop <- Space.gen(0)
        key <- Gen.chunkOfN(30)(Gen.char)
        validKey = key.filterNot(List('<', '>', '=').contains).mkString
        spacedKey = WhiteSpacedText(start, validKey, stop)
        value <- Gen.chunkOfN(30)(Gen.char)
        spacedValue = WhiteSpacedText(start, value.mkString, stop)
      } yield RandomAttribute(spacedKey, spacedValue)
  }
}
