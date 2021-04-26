package savinien.aoc18
package day06

import zio.*

import common.*
import common.point.*
import common.area.*
import parser.TokenParsers.*
import CoordinateHelpers.*

class CoordinatesService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 = 
    for
      data   <- input.getData
      points <- CoordinatesService.parsePoints(data)
      size   <- CoordinatesService.largestFiniteArea(points)
    yield AdventNumResult(size)

  override def part2 = 
    for
      data     <- input.getData
      max_dist <- input.getIntSetting("MaxDist", 10_000)
      points   <- CoordinatesService.parsePoints(data)
      size     <- CoordinatesService.getSumDistanceAreaSize(points, max_dist)
    yield AdventNumResult(size)

object CoordinatesService:
  private[day06] def parsePoints(input: String) =
    ZioParse.parseAllToZio(point.Parsers.point.lines)(input)

  private[day06] def largestFiniteArea(input: List[Point[Int]]): AdventTask[Int] =
    val values = getFiniteSizes(input).values
    if values.isEmpty then IO.fail(NoFiniteSize)
    else
      val (max, isSingle) = values.foldLeft((values.head, true)) {
        case ((max, isSingle), curr) =>
          if curr > max then      (curr, true)
          else if max > curr then (max, isSingle)
          else                    (max, false)
      }
      if !isSingle then IO.fail(TooManyEqualFiniteSize)
      else IO.succeed(max)

  private[day06] def getFiniteSizes(input: List[Point[Int]]): Map[Point[Int], Int] =
    val mArea = Area.fromIterable(input).flatMap(_.shrink(1)) 
    val mInfinites = getInfinites(input)
    mArea.zip(mInfinites).map {
      case (area, infinites) =>
        area.cells.foldLeft(Map.empty[Point[Int], Int]) {
          case (map, point) => findSingleClosest(input, point).map{
            closest =>
              if infinites.contains(closest) then map
              else map.updated(closest, map.getOrElse(closest, 0) + 1)
          }.getOrElse(map)
        }
    }.getOrElse(Map.empty)

  private[day06] def findSingleClosest(list: List[Point[Int]], point: Point[Int]): Option[Point[Int]] =
    list.tail.foldLeft((manhatten(point, list.head), Option(list.head))) {
      case ((min, best), curr) => 
        val dist = manhatten(point, curr)
        if dist < min then      (dist, Some(curr))
        else if dist > min then (min, best)
        else                    (min, None)
    }._2

  private[day06] def getInfinites(input: List[Point[Int]]): Option[Set[Point[Int]]] =
    Area.fromIterable(input).map { 
      _.perimeter.foldLeft(Set.empty[Point[Int]]) {
        (set, p) => findSingleClosest(input, p).map(set + _).getOrElse(set)
      }
    }

  private[day06] def getSumDistanceAreaSize(input: List[Point[Int]], maxDistance: Int): AdventTask[Int] =
    def countPath(path: List[Point[Int]], result: Int): Int = path match
      case Nil => result
      case head :: tail => 
        val current = input.map(i => manhatten(i, head)).sum
        if current < maxDistance then countPath(tail, result + 1)
        else countPath(tail, result)

    def countSumDistance(start: Point[Int], steps: Int, result: Int): Int =
      val current = countPath(start.diamond(steps).toList, 0)
      if current > 0 then countSumDistance(start + Point(1, 0), steps + 1, result + current)
      else result

    Area.fromIterable(input).map { area => 
      IO.succeed(countSumDistance(area.center, 0, 0))
    }.getOrElse(IO.fail(NoInput))

end CoordinatesService