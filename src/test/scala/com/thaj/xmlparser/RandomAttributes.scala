package com.thaj.xmlparser

import com.thaj.xmlparser.RandomAttributes.RandomAttribute
import zio.Chunk
import zio.test.Gen

final case class RandomAttributes private (value: Chunk[(RandomAttribute, Space)]) {
  override def toString: String = value.map { case (attribute, space) => s"$attribute$space" }.mkString
}

object RandomAttributes {

  def gen(minNumberOfAttributes: Int, maxNumberOfAttributes: Int): Gen[Any, RandomAttributes] =
    for {
      numberOfAttributes <- Gen.int(minNumberOfAttributes, maxNumberOfAttributes)
      randomAttributes <- Gen.chunkOfN(numberOfAttributes)(RandomAttribute.gen)
      spaces <- Gen.chunkOfN(numberOfAttributes)(Space.gen(1))
      value = randomAttributes
        .zip(spaces)
    } yield RandomAttributes(value)

  final case class RandomAttribute(
    key: WhiteSpacedText,
    value: WhiteSpacedText
  ) {

    def toAttribute: XmlObject.Attribute =
      (key.value, value.value)

    override def toString =
      s"$key=\"$value\""
  }

  object RandomAttribute {

    def gen =
      for {
        key <- WhiteSpacedText.gen
        value <- WhiteSpacedText.gen
      } yield RandomAttribute(key, value)
  }
}
