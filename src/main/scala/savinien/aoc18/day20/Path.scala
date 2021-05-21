package savinien.aoc18
package day20

import common.geometric.{Direction, Point}
import parsers.TokenParsers.*

case class Description(val waypoints: Map[Point[Int], Int], val exits: Map[Point[Int], Int]):
  def merge(other: Description): Description =
    Description(waypoints.mergeMin(other.waypoints), exits.mergeMin(other.exits))
  def emptyExits: Description = Description(waypoints, Map.empty)

object Description:
  val empty = Description(Map.empty, Map.empty)

extension (map: Map[Point[Int], Int])
  def updatedIfMin(point: Point[Int], value: Int): Map[Point[Int], Int] =
    if map.get(point).map(_ <= value).getOrElse(false) then map
    else map.updated(point, value)

  def mergeMin(other: Map[Point[Int], Int]): Map[Point[Int], Int] =
    if map.isEmpty then other
    else if other.isEmpty then map
    else map.foldLeft (other) { case (map, (point, length)) => map.updatedIfMin(point, length) }

trait Path:
  def show: String
  def getDescription(start: Point[Int], steps: Int): Description

private object ZeroPath extends Path:
  override def toString = ""
  override def show = "!"
  override def getDescription(start: Point[Int], length: Int) = Description(Map.empty, Map(start -> length))

private class LinearPath(val steps: List[Direction]) extends Path:
  override def toString = steps.map(_.short).mkString
  override def show = toString
  override def equals(other: Any) =
    if !other.isInstanceOf[LinearPath] then false
    else
      val o = other.asInstanceOf[LinearPath]
      steps.length == o.steps.length && steps.zip(o.steps).forall(_ == _)

  override def getDescription(start: Point[Int], length: Int): Description =
    val (exit, len, map) = steps.foldLeft (start, length, Map.empty[Point[Int], Int]) { case ((position, len, map), dir) =>
      val nextPosition = position + dir
      val nextLen = len + 1
      val nextMap = map.updatedIfMin(nextPosition, nextLen)
      (nextPosition, nextLen, nextMap)
    }
    Description(map, Map(exit -> len))

private class AlternativePath(val paths: List[Path]) extends Path:
  override def toString = s"(${paths.mkString("|")})"
  override def show = s"(${paths.map(_.show).mkString("|")})"
  override def equals(other: Any) =
    if !other.isInstanceOf[AlternativePath] then false
    else
      val o = other.asInstanceOf[AlternativePath]
      paths.length == o.paths.length && paths.zip(o.paths).forall(_ == _)

  override def getDescription(start: Point[Int], length: Int): Description =
    paths.tail.foldLeft (paths.head.getDescription(start, length)) { case (description, path) =>
      description.merge(path.getDescription(start, length))
    }

private class PathSequence(val paths: List[Path]) extends Path:
  override def toString = s"${paths.mkString}"
  override def show = s"[${paths.map(_.show).mkString("+")}]"
  override def equals(other: Any) =
    if !other.isInstanceOf[PathSequence] then false
    else
      val o = other.asInstanceOf[PathSequence]
      paths.length == o.paths.length && paths.zip(o.paths).forall(_ == _)

  override def getDescription(start: Point[Int], length: Int): Description =
    def exitLoop(path: Path, exits: List[(Point[Int], Int)], result: Description): Description =
      exits match
        case Nil => result
        case (exit, length) :: rest => 
          val nextDesc = result.merge(path.getDescription(exit, length))
          exitLoop(path, rest, nextDesc)

    def loop(paths: List[Path], result: Description): Description =
      paths match
        case Nil => result
        case path :: rest => 
          loop(rest, exitLoop(path, result.exits.toList, result.emptyExits))
    loop(paths, Description(Map.empty, Map(start -> length)))

object Path:
  val step = oneOf("ENWS") ^? checkedOption(Direction.fromChar)
  def linear: Parser[Path] = step.+ ^^ { steps => LinearPath(steps.toList) }

  def alternatives: Parser[Path] = (path.sepMany(char('|')) ~: char('|').?).inParens ^^ {
    case (Nil, _)             => ZeroPath
    case (first :: Nil, None) => first
    case (paths, None)        => AlternativePath(paths)
    case (paths, Some(_))     => AlternativePath(paths.appended(ZeroPath))
  }

  def path: Parser[Path] = (linear | alternatives).+ ^^ { list => 
    list.toList match
      case first :: Nil => first
      case _ :: _       => PathSequence(list.toList)
      case Nil          => ??? // this can never happen
  }

  def fullPath = char('^') *> path <* char('$')

object Walker:
  def furthestRoom(path: Path): Int =
    path.getDescription(Point.origin, 0).waypoints.values.max
  def doorCount(path: Path, minDoors: Int): Int =
    path.getDescription(Point.origin, 0).waypoints.values.count(_ >= minDoors)