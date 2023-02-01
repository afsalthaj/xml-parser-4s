package com.thaj.xmlparser

final case class OpenTag(
    whiteSpacedOpenBracket: String,
    text: WhiteSpacedText,
    attributes: RandomAttributes,
    whiteSpacedClosingBracket: String
) {
  override def toString: String = {
    s"${whiteSpacedOpenBracket}${text}${attributes}${whiteSpacedClosingBracket}"
  }

}

object OpenTag {
  def gen =
    for {
      start <- Bracket.Open.gen
      attributes <- RandomAttributes.gen
      text <- WhiteSpacedText.gen(
        minPreSpace = 0,
        minPostSpace = if (attributes.value.isEmpty) 0 else 1
      ) // a text should be suffixed by atleast 1 space if there is attribute key-value pair otherwise 0
      stop <- Bracket.Closed.gen
    } yield OpenTag(start, text, attributes, stop)
}
