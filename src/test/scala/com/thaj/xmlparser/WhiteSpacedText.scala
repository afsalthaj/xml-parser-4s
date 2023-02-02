package com.thaj.xmlparser

import zio.test.Gen

final case class WhiteSpacedText(value: String) {
  override def toString: String = value
}

object WhiteSpacedText {

  val gen: Gen[Any, WhiteSpacedText] =
    gen(0, 0)

  def gen(minPreSpace: Int, minPostSpace: Int): Gen[Any, WhiteSpacedText] =
    for {
      start <- Space.gen(minPreSpace)
      stop <- Space.gen(minPostSpace)
      chars <- Gen.chunkOfN(30)(Gen.char)
      text = chars.filterNot(InvalidTextCharacters.list.contains).mkString.trim
      withSpaces = s"$start$text$stop"
    } yield WhiteSpacedText(withSpaces)

}
