package savinien.aoc18
package day17

import parsers.TokenParsers.*
import common.geometric.Point
import common.geometric.CoordinateHelper.*

case class Range(from: Int, to: Int):
  def steps = to - from + 1

object Scanner:
  val range = (unsignedInteger <* string("..")) ~: unsignedInteger ^^ Range.apply
  def base(first: String, second: String) = (configValue(first, unsignedInteger) <* char(',').token) ~: configValue(second, range)
  val xStable = base("x", "y") ^^ { case (col, rowRange) => Point(col, rowRange.from).vertPath(rowRange.steps ).toList }
  val yStable = base("y", "x") ^^ { case (row, colRange) => Point(colRange.from, row).horizPath(colRange.steps ).toList }
  val complete = (xStable | yStable).lines ^? checkedOption(lines => Terrain(lines.flatten.toSet))