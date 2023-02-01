package com.thaj.xmlparser

import zio.test.Gen

sealed trait Bracket {
  def gen: Gen[Any, String] =
    Bracket.gen(this)

  override def toString: String =
    this match {
      case Bracket.Open        => "<"
      case Bracket.Closed      => ">"
      case Bracket.SlashedOpen => "</"
    }

}

object Bracket {
  case object Open extends Bracket
  case object Closed extends Bracket
  case object SlashedOpen extends Bracket

  def gen(bracket: Bracket): Gen[Any, String] =
    for {
      start <- Space.gen
      stop <- Space.gen
    } yield s"${start.toString}${bracket}${stop.toString}"
}
