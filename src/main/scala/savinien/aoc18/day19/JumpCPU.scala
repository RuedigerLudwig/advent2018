package savinien.aoc18
package day19

import parsers.TokenParsers.*
import parsers.Nel.NonEmptyList
import common.ParticalHelper.checkedOption

case class Instruction(operation: Operation, inputA: Int, inputB: Int, output: Int):
  def apply(register: Register): Register =
    operation.apply(inputA, inputB, output)(register)

  def getFactors(register: Register) =
    (operation.getA(inputA)(register), operation.getB(inputB)(register))

  override def toString = s"$operation $inputA $inputB $output"

object Instruction:
  def fromParameters(name: String, values: List[Int]): Option[Instruction] =
    val operation = Operation.allOperations.get(name)
    if operation.isEmpty then None
    else
      values match
        case inputA :: inputB :: output :: Nil =>
          if output < 0 || output >= 6 then None
          else if !operation.get.isDefinedAt(inputA, inputB, output) then None
          else Some(new Instruction(operation.get, inputA, inputB, output))
        case _ => None

enum Step(cpu: JumpCPU):
  case Running(cpu: JumpCPU) extends Step(cpu)
  case Finished(cpu: JumpCPU) extends Step(cpu)
  def get = cpu

case class JumpCPU private(val ipRegister: Int, val instructions: Vector[Instruction], val register: Register):
  val isRunning =
    val ip = register.get(ipRegister)
    0 <= ip && ip < instructions.length

  val instruction = 
    if isRunning then Some(instructions(register.get(ipRegister))) else None

  override def toString = 
    if isRunning then s"(${register.get(ipRegister)}) ${instruction.get} ${register.show}" 
    else s"Finished: ${register.show}"

  def setRegister(pos: Int, value: Int): JumpCPU =
    new JumpCPU(ipRegister, instructions, register.updated(pos, value))

  def tick: Step = 
    if !isRunning then Step.Finished(this)
    else Step.Running(new JumpCPU(ipRegister, instructions, instruction.get(register).inc(ipRegister)))

  final def runTillEnd: JumpCPU =
    def loop(current: JumpCPU): JumpCPU =
      current.tick match
        case Step.Running(cpu)  => loop(cpu)
        case Step.Finished(cpu) => cpu
    loop(this)

object JumpCPU:
  def fromValues(ip: Int, instructions: List[Instruction]): Option[JumpCPU] =
    if ip < 0 || ip >= 5 then None
    else if instructions.isEmpty then None
    else
      Some(new JumpCPU(ip, instructions.toVector, Register.empty(6)))

  val ip = string("#ip ") *> unsignedInteger.filter(_ < 6) <* space
  val instructionName = oneOf(NonEmptyList.fromIterable(Operation.allOperations.keys.map(s => string(s))).get).token
  val instruction = instructionName ~: unsignedInteger.token.repeatExact(3) <* endOfLine.* ^? checkedOption(Instruction.fromParameters)

  val parser = ip ~: instruction.* ^? checkedOption(fromValues)