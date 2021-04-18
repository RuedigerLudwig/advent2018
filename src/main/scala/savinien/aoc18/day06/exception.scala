package savinien.aoc18
package day06

import common.*

sealed trait CoordinatesException extends AdventException

case object NoInput extends CoordinatesException:
  override def toString() = "Did not get Input to work on"

case object NoFiniteSize extends CoordinatesException:
  override def toString() = "No finite sized area found"

case object TooManyEqualFiniteSize extends CoordinatesException:
  override def toString() = "To many equally finite areas found"
