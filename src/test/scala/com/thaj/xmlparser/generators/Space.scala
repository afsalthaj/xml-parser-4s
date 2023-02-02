package com.thaj.xmlparser.generators

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

object Space {

  val gen: Gen[Any, Space] =
    gen(0)

  def gen(minSpace: Int): Gen[Any, Space] =
    for {
      totalReturn <- Gen.int(minSpace, 1).map(Return)
      totalTab <- Gen.int(minSpace, 1).map(Tab)
      totalNextLine <- Gen.int(minSpace, 1).map(NextLine)
      totalWhiteSpace <- Gen.int(minSpace, 1).map(WhiteSpace)
      spaces <- Gen.chunkOfBounded(minSpace, 2)(
        Gen.oneOf(Gen.const(totalNextLine), Gen.const(totalWhiteSpace))
      )
    } yield Multiple(spaces)

  final case class NextLine(total: Int) extends Space
  final case class Return(total: Int) extends Space
  final case class Tab(total: Int) extends Space
  final case class WhiteSpace(total: Int) extends Space
  final case class Multiple(spaces: Chunk[Space]) extends Space
}
