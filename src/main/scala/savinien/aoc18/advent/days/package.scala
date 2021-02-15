package savinien.aoc18.advent

import zio._
import zio.macros.accessible

package object days {

  type SingleDay = Has[SingleDay.Service]

  @accessible
  object SingleDay {
    trait Service {
      def part1: Task[Unit]
      def part2: Task[Unit]
    }
  }
}
