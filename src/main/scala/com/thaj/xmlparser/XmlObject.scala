package com.thaj.xmlparser

import zio.Chunk

sealed trait XmlObject {
  def flattened: Map[Chunk[KeyComponent], String] = ???
}

object XmlObject {
  type Attribute = (String, String)

  final case class Text(value: String) extends XmlObject
  final case class TagElement(
      name: String,
      attributes: Chunk[Attribute],
      children: XmlObject
  ) extends XmlObject
}
