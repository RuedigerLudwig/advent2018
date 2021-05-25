package savinien.aoc18
package day16

import parsers.TokenParsers.*
import common.ParticalHelper.checkedOption

case class Register private(values: List[Int]):
  def apply(pos: Int) = values(pos)
  def isDefinedAt(pos: Int) = pos >=0 && pos < 4
  def updated(pos: Int, value: Int): Register =
    if !isDefinedAt(pos) then this
    else new Register(values.updated(pos, value))
  override def toString = s"${values.mkString(",")}"

  override def equals(other: Any): Boolean =
    other.asInstanceOf[Register].values.zip(values).forall(_ == _)

object Register:
  def initial: Register = Register(List.fill(4)(0))
  def fromList(values: List[Int]): Option[Register] = 
    if values.length != 4 then None
    else Some(Register(values))

case class Instruction(opCode: Int, inputA: Int, inputB: Int, output: Int)
object Instruction:
  def fromList(values: List[Int]): Option[Instruction] =
    values match
      case opCode :: inputA :: inputB :: output :: Nil =>
        if opCode < 0 || opCode >= 16 then None
        else if output < 0 || output >= 4 then None
        else Some(new Instruction(opCode, inputA, inputB, output))
      case _ => None

case class Sample(before: Register, instruction: Instruction, after: Register) :
  def getPossibleOpCodes(operations: Set[Operation]): Set[Operation] =
    operations.filter(_.apply(instruction)(before) == after)

  def countPossibleOpCodes(operations: Set[Operation]): Int = getPossibleOpCodes(operations).size

object Sample:
  val registers = unsignedInteger.sepExact(char(',').token)(4).inSquares.token ^? checkedOption(Register.fromList)

  val before = string("Before:").token *> registers <* endOfLine.*
  val after = string("After:").token *> registers <* endOfLine.*
  val instruction = unsignedInteger.token.repeatExact(4) <* endOfLine.* ^? checkedOption(Instruction.fromList)

  val sample = before ~: instruction ~: after ^^ Sample.apply
  val complete = sample.* ~: instruction.*