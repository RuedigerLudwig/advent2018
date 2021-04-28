package savinien.aoc18
package day10

import common.point.Point
import parsers.TokenParsers.*
import common.*
import common.area.Area
import common.point.Point.*
import math.Ordering.Implicits.infixOrderingOps

case class Star[T: Integral](val position: Point[T], val velocity: Point[T]):
  def twinkle: Star[T] =
    Star(position + velocity, velocity)

object Star:
  def starParser[T: Integral]: Parser[Star[T]] = 
    configValue("position", Point.parser.inAngles) ~: 
    configValue("velocity", Point.parser.inAngles) ^^ Star.apply

  def fromString[T: Integral](input: String) = ZioParse.parseAllToZio(starParser[T])(input)

object Stars:
  def fromStringList[T: Integral](input: String): AdventTask[List[Star[T]]] =
    ZioParse.parseAllToZio(Star.starParser.lines)(input)

  extension [T: Integral](stars: List[Star[T]])
    def getArea: Option[Area[T]] =
      Area.fromIterable(stars.map(_.position))

    def twinkleAll: List[Star[T]] = stars.map(_.twinkle)

    def containsStar(point: Point[T]): Boolean = stars.exists(_.position == point)

    def minimize: (List[Star[T]], Int) =
      def find_smaller(curr: List[Star[T]], prev: List[Star[T]], prevArea: Area[T], seconds: Int): (List[Star[T]], Int) =
        val currArea = curr.getArea.get
        val ps = prevArea.size
        val cs = currArea.size
        if currArea.size > prevArea.size then (prev, seconds)
        else find_smaller(curr.twinkleAll, curr, currArea, seconds + 1)

      getArea.map(find_smaller(twinkleAll, stars, _, 0)).getOrElse((stars, 0))

    def show: String =
      getArea.map { 
        _.rows.map {
          _.map { point => if containsStar(point) then '#' else ' ' }.mkString
        }.toList.mkString("\n")
      }.getOrElse("")