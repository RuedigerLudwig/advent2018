package savinien.aoc18
package day13

import common.*
import zio.*
import common.geometric.Point

case class MineCartService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 = 
    for
      data    <- input.getData
      mineMap <- MineMap.fromString(data)
      site    <- MineCartService.moveToCrash(mineMap, 100_000)
    yield AdventStringResult(s"${site.x},${site.y}")

  override def part2 = 
    for
      data    <- input.getData
      mineMap <- MineMap.fromString(data)
      site    <- MineCartService.moveTillLast(mineMap, 100_000)
    yield AdventStringResult(s"${site.x},${site.y}")

object MineCartService:
  def moveToCrash(mineMap: MineMap, maxIterations: Int): AdventTask[Point[Int]] =
    if maxIterations == 0 then ZIO.fail(NoCrashHappened)
    else
      val nextMap = mineMap.oneTick
      if !nextMap.crashSites.isEmpty then ZIO.succeed(nextMap.crashSites.last)
      else moveToCrash(nextMap, maxIterations - 1)

  def moveTillLast(mineMap: MineMap, maxIterations: Int): AdventTask[Point[Int]] =
    if maxIterations == 0 then ZIO.fail(TooManyCarts(mineMap.carts.size))
    else
      val nextMap = mineMap.oneTick
      if nextMap.carts.size == 1 then ZIO.succeed(nextMap.carts.head.position)
      else if nextMap.carts.isEmpty then ZIO.fail(ALlCartsCrashed)
      else moveTillLast(nextMap, maxIterations - 1)

end MineCartService