package savinien
package aoc18
package common

import zio._

type SingleDay = Has[SingleDay.Service]

object SingleDay:
  trait Service:
    def part1: AdventTask[AdventResult]
    def part2: AdventTask[AdventResult]

  def part1: ZIO[SingleDay, AdventException, AdventResult] =
    ZIO.accessM(_.get.part1)

  def part2: ZIO[SingleDay, AdventException, AdventResult] =
    ZIO.accessM(_.get.part2)