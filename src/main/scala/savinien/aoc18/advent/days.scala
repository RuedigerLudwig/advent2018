package savinien
package aoc18
package advent

import zio._

import aoc18.day01

package object days {
  def MAX_DAY = 1

  def getDay(day: Int) = day match {
    case 1 => day01.live
  }

  type SingleDay = Has[SingleDay.Service]

  object SingleDay {
    trait Service {
      def part1: Task[Unit]
      def part2: Task[Unit]
    }

    def part1: ZIO[SingleDay, Throwable, Unit] =
      ZIO.accessM(_.get.part1)

    def part2: ZIO[SingleDay, Throwable, Unit] =
      ZIO.accessM(_.get.part2)
  }
}
