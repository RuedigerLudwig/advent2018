package savinien.aoc18
package day17

import annotation.targetName
import common.geometric.{Point, Area, Direction}

case class Water(val filled: Set[Point[Int]], flow: Set[Point[Int]]):
  lazy val driedUp: Set[Point[Int]] = flow.diff(filled)
  lazy val reach: Set[Point[Int]] = filled ++ flow

  @targetName("addWater")
  def `+`(other: Water): Water =
    new Water(filled ++ other.filled, flow ++ other.flow)

object Water:
  val empty = new Water(Set.empty, Set.empty)
  def asFilled(filled: Iterable[Point[Int]]) = new Water(filled.toSet, Set.empty)
  def asFlow(flow: Iterable[Point[Int]]) = new Water(Set.empty, flow.toSet)