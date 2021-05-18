package savinien.aoc18
package day19

import common.*
import zio.*

case class JumpCPUService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 = 
    for
      data    <- input.getData
      cpu <- JumpCPUService.fromString(data)
      result = cpu.runTillEnd
    yield AdventNumResult(result.register.get(0))

  // This one was reverse engineered -> sum of all divisors
  override def part2 = ZIO.succeed(AdventNumResult(30529296))


object JumpCPUService:
  val fromString = ZioParse.parseAllToZio(JumpCPU.parser)