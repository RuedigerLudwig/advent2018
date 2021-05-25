package savinien.aoc18
package day18

import parsers.TokenParsers.*
import common.geometric.{Point, Area}
import common.ParticalHelper.checkedOption

import annotation.{targetName, tailrec}

case class Acre (val trees: Int, val lumberyards: Int) derives CanEqual:
  @targetName("opAddAcre")
  def `+`(other: Acre): Acre = Acre(trees + other.trees, lumberyards + other.lumberyards)

  @targetName("opSubAcre")
  def `-`(other: Acre): Acre = Acre(trees - other.trees, lumberyards - other.lumberyards)

object Acre:
  val open = Acre(0, 0)
  val trees = Acre(1, 0)
  val lumberyard = Acre(0, 1)

class Landscape(val map: Map[Point[Int], Acre], cols: Int, rows: Int) derives CanEqual:
  override def toString =
    Area(Point(0, 0), Point(cols - 1, rows - 1)).rows.map {
      _.map {
        point => get(point) match
          case Acre.open => '.'
          case Acre.trees => '|'
          case Acre.lumberyard => '#'
          case _ => ()
      }.mkString
    }.mkString("\n")

  override def equals(other: Any): Boolean = 
    if !other.isInstanceOf[Landscape] then false
    else
      val o = other.asInstanceOf[Landscape]
      map.size == o.map.size && map.forall((key, value) => o.map.get(key).map(_ == value).getOrElse(false))

  def value: Int =
    val result = map(Point(cols - 1, rows - 1))
    result._1 * result._2

  private def get(coord: Point[Int]): Acre =
    val current = map(coord)
    val north = map.getOrElse(coord + Point(0, -1), Acre.open)
    val west = map.getOrElse(coord + Point(-1, 0), Acre.open)
    val northwest = map.getOrElse(coord + Point(-1, -1), Acre.open)
    current + northwest - north - west

  private[day18] def getNeighborhood(coord: Point[Int]): Acre =
    val (southeastX, deltaX) = if coord.x < cols - 1 then (coord.x + 1, -3) else (coord.x, -2)
    val (southeastY, deltaY) = if coord.y < rows - 1 then (coord.y + 1, -3) else (coord.y, -2)
    val corner = Point(southeastX, southeastY)
    val southeast = map(corner)
    val north = map.getOrElse(corner + Point(0, deltaY), Acre.open)
    val west = map.getOrElse(corner + Point(deltaX, 0), Acre.open)
    val northwest = map.getOrElse(corner + Point(deltaX, deltaY), Acre.open)
    southeast + northwest - north - west

  def tick: Landscape =
    val nextMap = Area(Point(0, 0), Point(cols - 1, rows - 1)).cells.foldLeft (Map.empty[Point[Int], Acre]) {
      (map, coord) => 
        val neighborhood = getNeighborhood(coord)
        val nextAcre = get(coord) match
          case Acre.open if neighborhood.trees >= 3 =>  Acre.trees
          case Acre.trees if neighborhood.lumberyards >= 3 => Acre.lumberyard
          case Acre.lumberyard if neighborhood.trees == 0 || neighborhood.lumberyards == 1 => Acre.open
          case other => other
        Landscape.updateMap(map, coord, nextAcre)
    }
    new Landscape(nextMap, cols, rows)

  final def ticks(times: Int): Landscape =
    @tailrec
    def loop(landscape: Landscape, repeat: Int, previous: Map[Int, Landscape]): Landscape =
      if times - repeat < 0 then landscape
      else 
        val nextLandscape = landscape.tick
        if nextLandscape == landscape then landscape
        else previous.find(_._2 == nextLandscape) match
          case None => loop(nextLandscape, repeat + 1, previous.updated(repeat, nextLandscape))
          case Some((prevRepeat, _)) => 
            val steps = repeat - prevRepeat
            val missing = (times - prevRepeat) % steps
            previous(prevRepeat + missing)

    loop(this, 1, Map(0 -> this))

object Landscape:
  val parser = Tile.parser.*.lines ^? checkedOption(Landscape.fromMap)

  private def updateMap(map: Map[Point[Int], Acre], coord: Point[Int], element: Acre): Map[Point[Int], Acre] =
    val north = map.getOrElse(coord + Point(0, -1), Acre.open)
    val west = map.getOrElse(coord + Point(-1, 0), Acre.open)
    val northwest = map.getOrElse(coord + Point(-1, -1), Acre.open)
    map.updated(coord, element + north + west - northwest)

  def fromMap(tileMap: List[List[Tile]]): Option[Landscape] = 
    var maybeColCount = Option.empty[Option[Int]]

    val rawMap = tileMap.zipWithIndex.map {
      (row, rowIndex) => {
        if maybeColCount.isEmpty then maybeColCount = Some(Some(row.length))
        else maybeColCount = maybeColCount.map(cc => if cc.isEmpty || cc.get == row.length then cc else None)
        row.zipWithIndex.map {
          (tile, colIndex) => Point(colIndex, rowIndex) -> tile
        }
      }
    }.flatten.toMap

    if maybeColCount.isEmpty || maybeColCount.get.isEmpty then None
    else
      val rowCount = tileMap.length
      val colCount = maybeColCount.get.get
      val map = Area(Point(0, 0), Point(colCount - 1, rowCount - 1)).cells.foldLeft (Map.empty[Point[Int], Acre]) {
        (map, coord) => 
          val element = rawMap(coord) match
            case Tile.Open => Acre.open
            case Tile.Trees => Acre.trees
            case Tile.Lumberyard => Acre.lumberyard

          updateMap(map, coord, element) 
      }
      Some(Landscape(map, colCount, rowCount))