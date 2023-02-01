package com.thaj.xmlparser

import zio.test.Gen

final case class RandomAttributes private (value: List[String]) {
  override def toString: String = value.mkString
}

object RandomAttributes {
  def gen: Gen[Any, RandomAttributes] =
    for {
      numberOfAttributes <- Gen.int(0, 10)
      randomAttributes <- Gen.listOfN(numberOfAttributes)(RandomAttribute.gen)
      spaces <- Gen.listOfN(numberOfAttributes)(Space.gen(1))
      value = randomAttributes
        .zip(spaces)
        .map({ case (attribute, space) => s"${attribute}${space}" })
    } yield RandomAttributes(value)

  final case class RandomAttribute(
      key: WhiteSpacedText,
      value: WhiteSpacedText
  ) {
    override def toString =
      s"${key}=\"${value}\""
  }

  object RandomAttribute {
    def gen =
      for {
        key <- WhiteSpacedText.gen
        value <- WhiteSpacedText.gen
      } yield RandomAttribute(key, value)
  }
}
