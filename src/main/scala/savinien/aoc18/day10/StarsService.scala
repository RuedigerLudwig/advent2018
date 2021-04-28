package savinien.aoc18
package day10

import common.*
import zio.*
import Stars.*

class StarsService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 = 
    for
      data  <- input.getData
      stars <- Stars.fromStringList[Long](data)
    yield AdventStringResult(stars.minimize._1.show)

  override def part2 = 
    for
      data  <- input.getData
      stars <- Stars.fromStringList[Long](data)
    yield AdventNumResult(stars.minimize._2)

object StarsService:
end StarsService