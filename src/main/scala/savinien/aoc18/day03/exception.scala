package savinien.aoc18
package day03

import common.*

sealed trait MatterException extends AdventException

case class NoLegalClaim(claim: String) extends MatterException:
  override def toString() = s"No Legal claim: $claim"

case object NoSolitaireFound extends MatterException:
  override def toString() = s"Did not find any Solitaire claims"

case object TooManySolitaireFound extends MatterException:
  override def toString() = s"Found more than one solitaire Claims"