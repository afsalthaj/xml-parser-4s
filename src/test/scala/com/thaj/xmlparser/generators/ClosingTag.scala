package com.thaj.xmlparser.generators

import zio.test.Gen

final case class ClosingTag(
    open: String,
    internalText: WhiteSpacedText,
    closed: String
) {
  override def toString: String =
    s"${open}${internalText}${closed}"
}

object ClosingTag {
  def gen(tagName: WhiteSpacedText): Gen[Any, ClosingTag] =
    for {
      start <- Bracket.SlashedOpen.gen
      stop <- Bracket.Closed.gen
    } yield ClosingTag(start, tagName, stop)
}
