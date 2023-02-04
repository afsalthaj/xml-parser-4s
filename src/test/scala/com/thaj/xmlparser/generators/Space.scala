package com.thaj.xmlparser.generators

import com.thaj.xmlparser.Parsers
import zio.test.Gen

sealed trait Space {

  def print: String =
    this match {
      case Space.WhiteSpace(char) =>
        Printer.print(char.toString)
      case Space.Multiple(total, space) =>
        Printer.print(List.fill(total)(space.print): _*)
    }
}

object Space extends Parsers {

  val gen: Gen[Any, Space] =
    gen(0)

  def gen(minSpace: Int): Gen[Any, Space] =
    for {
      totalSpaces <- Gen.int(minSpace, 10)
      charSpace <- Gen.oneOf(UnicodeEmptyCharacters.map(Gen.const(_)): _*)
    } yield Multiple(totalSpaces, WhiteSpace(charSpace))

  final case class WhiteSpace(char: Char) extends Space
  final case class Multiple(total: Int, spaces: Space) extends Space
}
