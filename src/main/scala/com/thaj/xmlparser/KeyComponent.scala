package com.thaj.xmlparser

// Already exist in zio-config 4.x
sealed trait KeyComponent

object KeyComponent {
  case class KeyName(value: String) extends KeyComponent

  case class Index(value: Int) extends KeyComponent
}
