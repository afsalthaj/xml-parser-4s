package com.thaj.xmlparser.generators

import com.thaj.xmlparser.Parsers
import zio.Chunk
import zio.test.Gen

sealed trait Space {

  def print: String =
    this match {
      case Space.NextLine(total) =>
        Printer.print(List.fill(total)("\n"): _*)
      case Space.WhiteSpace(total) =>
        Printer.print(List.fill(total)(" "): _*)
      case Space.Tab(total) =>
        Printer.print(List.fill(total)("\t"): _*)
      case Space.Return(total) =>
        Printer.print(List.fill(total)("\r"): _*)
      case Space.Multiple(spaces) =>
        Printer.print(spaces.map(_.print): _*)
    }
}

object Space extends Parsers{

  val gen: Gen[Any, Space] =
    gen(0)

  def gen(minSpace: Int): Gen[Any, Space] =
    for {
      totalSpaces <- Gen.int(minSpace, 10)

      spaces <- Gen.chunkOfBounded(minSpace, 10)(
        Gen.oneOf(UnicodeEmptyCharacters.map(Gen.const(_)) :_*)
      )
    } yield Multiple(spaces)

  final case class WhiteSpace(total: Int, char: Char) extends Space
  final case class Multiple(spaces: Chunk[Space]) extends Space
}
