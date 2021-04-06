package savinien.aoc18
package day05

import common.*

sealed trait PolymerException extends AdventException

case object NoMinimum extends PolymerException:
  override def toString() = "Did not find any Minimum"