package savinien.aoc18
package day11

import common.geometric.*
import CoordinateHelper.*

class PowerGrid(val size: Int, val serial: Int):
  val grid = getSummedPowerGrid

  private[day11] def getSummedPowerGrid: Map[Point[Int], Long] =
    val grid = collection.mutable.Map.empty[Point[Int], Long]
    for
      point <- Area.square(Point(1, 1), size).get.cells
    do 
      val power = PowerGrid.calcPower(serial)(point)
      val p2 = grid.getOrElse(point - Point(1, 0), 0L) 
      val p3 = grid.getOrElse(point - Point(0, 1), 0L) 
      val p4 = grid.getOrElse(point - Point(1, 1), 0L)
      grid.update(point, power + p2 + p3 - p4)
    grid.toMap

  private[day11] def getSumFor(cellSize: Int)(point: Point[Int]): (Point[Int], Long) =
    val outPoint = point - Point(1, 1)
    val p1 = grid.getOrElse(outPoint, 0L)
    val p2 = grid.getOrElse(outPoint + Point(cellSize, 0), 0L) 
    val p3 = grid.getOrElse(outPoint + Point(0, cellSize), 0L) 
    val p4 = grid.getOrElse(outPoint + Point(cellSize, cellSize), 0L)
    point -> (p1 + p4 - p2 - p3)

  def getPowerSums(cellSize: Int): Iterable[(Point[Int], Long)] =
    def sumTo = size - cellSize + 1
    Area.square(Point(1, 1), sumTo).get.cells.map { getSumFor(cellSize) }

  def getMaxPowerFor(cellSize: Int): List[(Point[Int], Long)] =
    assert(cellSize > 0 && cellSize <= size)
    val sumGrid = getPowerSums(cellSize).toList
    val max = sumGrid.maxBy(_._2)._2
    sumGrid.filter { _._2 == max }.toList

  def getMaxPower: Option[(Point[Int], Int, Long)] =
    val (best, max) = (1 until size).foldLeft ((Option.empty[(Point[Int], Int)], -6L)) {
      case ((best, max), cellSize) =>
        val sumGrid = getPowerSums(cellSize).toList
        val newMax = sumGrid.maxBy(_._2)._2
        if newMax < max then (best, max)
        else if newMax > max then
          sumGrid.filter { _._2 == newMax }.toList match
            case (newBest, _) :: Nil => (Some((newBest, cellSize)), newMax)
            case _ => (None, newMax)
        else (None, max)
    }
    best.map((point, cellSize) => (point, cellSize, max))

object PowerGrid:
  private[day11] def calcPower(serial: Int)(point: Point[Int]): Long =
    val rackId = (point.x + 10).toLong
    var power = (rackId * point.y + serial) * rackId
    var result = power / 100 % 10 - 5
    result