package savinien
package aoc18
package advent

import zio._

import aoc18.day01
import aoc18.day02

object Days:
  def MAX_DAY = 2

  def getDay(day: Int) = day match
    case 1 => day01.live
    case 2 => day02.live

type SingleDay = Has[SingleDay.Service]

object SingleDay:
  trait Service:
    def part1: IO[AdventException, AdventResult]
    def part2: IO[AdventException, AdventResult]

  def part1: ZIO[SingleDay, AdventException, AdventResult] =
    ZIO.accessM(_.get.part1)

  def part2: ZIO[SingleDay, AdventException, AdventResult] =
    ZIO.accessM(_.get.part2)