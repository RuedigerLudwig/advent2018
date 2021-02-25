package savinien
package aoc18
package common

import zio._

type SingleDay = Has[SingleDay.Service]

object SingleDay:
  trait Service:
    def part1: IO[AdventException, AdventResult]
    def part2: IO[AdventException, AdventResult]

  def part1: ZIO[SingleDay, AdventException, AdventResult] =
    ZIO.accessM(_.get.part1)

  def part2: ZIO[SingleDay, AdventException, AdventResult] =
    ZIO.accessM(_.get.part2)