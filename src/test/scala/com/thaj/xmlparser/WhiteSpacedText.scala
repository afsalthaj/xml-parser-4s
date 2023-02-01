package com.thaj.xmlparser

import zio.Chunk
import zio.test.Gen

final case class WhiteSpacedText(value: String) {
  override def toString: String = value
}

object WhiteSpacedText {
  def gen =
    for {
      start <- Space.gen
      stop <- Space.gen
      chars <- Gen.chunkOfN(30)(Gen.char)
      text = prefixEscape(chars).mkString.trim
      withSpaces = s"${start}${text}${stop}"
    } yield WhiteSpacedText(withSpaces)

  def prefixEscape(chars: Chunk[Char]): Chunk[Char] =
    chars.flatMap(char =>
      if (InvalidTextCharacters.list.contains(char)) Chunk('\\', char)
      else Chunk(char)
    )

}
