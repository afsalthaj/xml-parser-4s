package com.thaj.xmlparser

import zio.test.Gen

final case class WhiteSpacedText(preSpace: Space, value: String, postSpace: Space) {
  override def toString: String = s"$preSpace$value$postSpace"

  def toXmlObjectText: XmlObject =
    XmlObject.Text(value)
}

object WhiteSpacedText {

  val gen: Gen[Any, WhiteSpacedText] =
    gen(0, 0)

  def gen(minPreSpace: Int, minPostSpace: Int): Gen[Any, WhiteSpacedText] =
    for {
      start <- Space.gen(minPreSpace)
      stop <- Space.gen(minPostSpace)
      chars <- Gen.chunkOfN(30)(Gen.const('a'))
      text = chars.filterNot(InvalidTextCharacters.list.contains).mkString.trim
    } yield WhiteSpacedText(start, text, stop)

}
