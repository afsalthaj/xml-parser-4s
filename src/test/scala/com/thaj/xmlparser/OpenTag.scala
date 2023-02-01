package com.thaj.xmlparser

final case class OpenTag(
    open: String,
    internalText: WhiteSpacedText,
    closed: String
) {
  override def toString: String =
    s"${open}${internalText}${closed}"
}

object OpenTag {
  def gen =
    for {
      start <- Bracket.Open.gen
      text <- WhiteSpacedText.gen
      stop <- Bracket.Closed.gen
    } yield OpenTag(start, text, stop)
}
