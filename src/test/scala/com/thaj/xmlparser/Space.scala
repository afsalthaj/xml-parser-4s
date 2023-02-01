package com.thaj.xmlparser

import zio.Chunk
import zio.test.Gen

sealed trait Space {
  override def toString: String =
    this match {
      case Space.NextLine(total)   => List.fill(total)("\n").mkString
      case Space.WhiteSpace(total) => List.fill(total)(" ").mkString
      case Space.Multiple(spaces)  => spaces.map(_.toString).mkString
    }
}

object Space {
  val gen: Gen[Any, Space] =
    gen(0)

  def gen(minSpace: Int): Gen[Any, Space] =
    for {
      totalNextLine <- Gen.int(minSpace, 1).map(NextLine)
      totalWhiteSpace <- Gen.int(minSpace, 1).map(WhiteSpace)
      spaces <- Gen.chunkOfBounded(minSpace, 2)(
        Gen.oneOf(Gen.const(totalNextLine), Gen.const(totalWhiteSpace))
      )
    } yield Multiple(spaces)

  case class NextLine(total: Int) extends Space
  case class WhiteSpace(total: Int) extends Space
  case class Multiple(spaces: Chunk[Space]) extends Space
}
