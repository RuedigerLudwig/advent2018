package savinien.aoc18
package day21

import parsers.TokenParsers.*
import parsers.Nel.NonEmptyList
import annotation.tailrec
import savinien.aoc18.day19.JumpCPU
import savinien.aoc18.day21.UnderflowCPU
import common.ParticalHelper.checkedOption

case class Instruction(operation: Operation, inputA: Int, inputB: Int, output: Int):
  def apply(register: Register): Register =
    register.updated(output, getResult(register))

  def getResult(register: Register): Long =
    val (a, b) = getFactors(register)
    operation.calc(a, b)

  def getFactors(register: Register) =
    (operation.getA(inputA)(register), operation.getB(inputB)(register))

  override def toString = s"$operation $inputA $inputB $output"

object Instruction:
  def fromParameters(operation: Operation, values: List[Int]): Option[Instruction] =
    values match
      case inputA :: inputB :: output :: Nil =>
        if output < 0 || output >= Register.length then None
        else if !operation.isDefinedAt(inputA, inputB) then None
        else Some(new Instruction(operation, inputA, inputB, output))
      case _ => None

enum Step(cpu: UnderflowCPU):
  case Running(cpu: UnderflowCPU) extends Step(cpu)
  case Output(cpu: UnderflowCPU, output: Long) extends Step(cpu)
  case Finished(cpu: UnderflowCPU) extends Step(cpu)
  def get = cpu

case class UnderflowCPU private(val ipRegister: Int, val instructions: Vector[Instruction], val register: Register):
  val isRunning =
    val ip = register.get(ipRegister)
    0 <= ip && ip < instructions.length

  val instruction = 
    if isRunning then Some(instructions(register.get(ipRegister).asInstanceOf[Int])) else None

  override def toString = 
    if isRunning then s"(${register.get(ipRegister)}) ${instruction.get} ${register.show}" 
    else s"Finished: ${register.show}"

  def setRegister(pos: Int, value: Int): UnderflowCPU =
    new UnderflowCPU(ipRegister, instructions, register.updated(pos, value))

  def tick: Step = 
    if !isRunning then Step.Finished(this)
    else 
      val cpu = new UnderflowCPU(ipRegister, instructions, instruction.get(register).inc(ipRegister))
      if cpu.register.get(6) == 0 then Step.Running(cpu)
      else Step.Output(cpu.setRegister(6, 0), cpu.register.get(6))

  private def runLoop(previous: List[Long]): Option[Long] =
    tick match
      case Step.Running(cpu)  => cpu.runLoop(previous)
      case Step.Finished(_) => None
      case Step.Output(cpu, output) =>
        if previous.contains(output) then Some(previous.head)
        else cpu.runLoop(output :: previous)

  final def runForMin: Option[Long] =
    tick match
      case Step.Running(cpu)  => cpu.runForMin
      case Step.Finished(_) => None
      case Step.Output(_, output) => Some(output)

  final def runForMax: Option[Long] =
    runLoop(List.empty)

object UnderflowCPU:
  def fromValues(ip: Int, instructions: List[Instruction]): Option[UnderflowCPU] =
    if ip < 0 || ip >= Register.length then None
    else if instructions.isEmpty then None
    else Some(new UnderflowCPU(ip, instructions.toVector, Register.empty))

  val ip = string("#ip ") *> unsignedInteger.filter(_ < Register.length) <* space
  val operation: Parser[Operation] = string(4) ^? checkedOption(Operation.fromString)
  val instruction = operation ~: unsignedInteger.token.repeatExact(3) <* endOfLine.* ^? checkedOption(Instruction.fromParameters)

  val parser = ip ~: instruction.* ^? checkedOption(fromValues)