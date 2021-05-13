package savinien.aoc18
package day16

import common.*
import zio.*
import parsers.TokenParsers.*

case class OpCodeService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 = 
    for
      data    <- input.getData
      samples <- OpCodeService.readSample(data)
      result  <- ZIO.succeed(samples.count(_.countPossibleOpCodes(Operation.allOperations) >= 3))
    yield AdventNumResult(result)

  override def part2 =
    for
      data                    <- input.getData
      (samples, instructions) <- OpCodeService.readComplete(data)
      opCodes                 <- OpCodeService.getCorrectOpcodes(samples)
    yield AdventNumResult(OpCodeService.applyCalculation(instructions, opCodes)(0))

object OpCodeService:
  def readSample(input: String) = ZioParse.parseToZio(Sample.sample.*)(input)
  def readComplete(input: String) = ZioParse.parseAllToZio(Sample.complete)(input)

  def getCorrectOpcodes(samples: List[Sample]): AdventTask[Map[Int, Operation]] =
    def removeOperation(opMap: Map[Int, Set[Operation]], operation: Operation): Map[Int, Set[Operation]] =
      opMap.map((op, set) => (op, set.excl(operation)))

    def reduceMap(opMap: Map[Int, Set[Operation]], reduced: Map[Int, Operation]): AdventTask[Map[Int, Operation]] =
      opMap.find(_._2.size == 1) match
        case None => ZIO.fail(NotReducable)
        case Some((opCode, set)) =>
          val operation = set.head
          val nextReduced = reduced.updated(opCode, operation)
          val unprocessed = removeOperation(opMap.removed(opCode), operation)
          if unprocessed.isEmpty then ZIO.succeed(nextReduced)
          else reduceMap(unprocessed, nextReduced)

    val operations = (0 until 16).map(_ -> Operation.allOperations).toMap
    val fittingOperations = samples.foldLeft (operations) {
      case (allOperations, sample) =>
        val currentOpCode = sample.instruction.opCode
        val prevOperations = allOperations(currentOpCode)
        val operations = sample.getPossibleOpCodes(prevOperations)
        if operations.isEmpty then
          println("Empty!")
        allOperations.updated(currentOpCode, operations)
    }

    reduceMap(fittingOperations, Map.empty)


  def applyCalculation(instructions: List[Instruction], opCodes: Map[Int, Operation]) = 
    instructions.foldLeft (Register.initial) {
      case (register, instruction) => opCodes(instruction.opCode)(instruction)(register)
    }