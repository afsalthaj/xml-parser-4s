package com.thaj.xmlparser

import zio.Chunk

// To be moved to zio-parser
sealed trait XmlObject {
  def flattened: Map[Chunk[KeyComponent], String] = ???
}

object XmlObject {
  type Attribute = (String, String)

  final case class Text(value: String) extends XmlObject
  final case class TagElement(
      name: String,
      attributes: Chunk[Attribute],
      children: Chunk[XmlObject]
  ) extends XmlObject
}
