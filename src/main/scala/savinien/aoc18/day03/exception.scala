package savinien.aoc18
package day03

import common._

sealed trait MatterException extends AdventException

case class NoLegalClaim(claim: String) extends MatterException:
  override def toString() = s"No Legal claim: $claim"

case object NoSolitaireFound extends MatterException:
  override def toString() = s"Did not find any Solitaire claims"

case class TooManySolitaireFound(number : Int) extends MatterException:
  override def toString() = s"Found $number solitaire Claims"