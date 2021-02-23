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
    def part1: UIO[Unit]
    def part2: UIO[Unit]

  def part1: URIO[SingleDay,  Unit] =
    ZIO.accessM(_.get.part1)

  def part2: URIO[SingleDay, Unit] =
    ZIO.accessM(_.get.part2)
