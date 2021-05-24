package savinien.aoc18
package day21

import common.*
import zio.*

case class UnderflowService(input: AdventInput.Service) extends SingleDay.Service:
  // The input was changed at two positions: line 19 has a div to speed to iterations
  // Line 30 was added to allow output of the expected value
  override def part1 = 
    for
      data <- input.getData
      cpu  <- UnderflowService.fromString(data)
    yield AdventNumResult(cpu.runForMin.get)

  override def part2 = 
    for
      data <- input.getData
      cpu  <- UnderflowService.fromString(data)
    yield AdventNumResult(cpu.runForMax.get)

object UnderflowService:
  val fromString = ZioParse.parseAllToZio(UnderflowCPU.parser)