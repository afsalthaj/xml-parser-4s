package com.thaj.xmlparser

final case class Tags(openTag: OpenTag, closingTag: ClosingTag)

object Tags {
  val gen =
    for {
      open <- OpenTag.gen
      closed <- ClosingTag.gen(open.internalText)
    } yield Tags(open, closed)
}
