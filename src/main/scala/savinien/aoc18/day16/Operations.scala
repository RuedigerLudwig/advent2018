package savinien.aoc18
package day16

sealed trait Operation:
  def apply(instructions: Instruction): Register => Register
  def isDefinedAt(instructions: Instruction): Boolean

case object SetR extends Operation:
  def apply(instruction: Instruction): Register => Register =
    register => register.updated(instruction.output, register(instruction.inputA))
  override def isDefinedAt(instruction: Instruction) =
    instruction.inputA >= 0 && instruction.inputA < 4
  override def toString = "setr"

case object SetI extends Operation:
  def apply(instruction: Instruction): Register => Register =
    register => register.updated(instruction.output, instruction.inputA)
  override def isDefinedAt(instruction: Instruction) = true
  override def toString = "seti"

sealed trait RROperation(name: String, calc: (Int, Int) => Int) extends Operation:
  def apply(instruction: Instruction): Register => Register =
    register => register.updated(instruction.output, calc(register(instruction.inputA), register(instruction.inputB)))
  override def isDefinedAt(instruction: Instruction) =
    instruction.inputA >= 0 && instruction.inputA < 4 && instruction.inputB >= 0 && instruction.inputB < 4
  override def toString = name

sealed trait RIOperation(name: String, calc: (Int, Int) => Int) extends Operation:
  def apply(instruction: Instruction): Register => Register =
    register => register.updated(instruction.output, calc(register(instruction.inputA), instruction.inputB))
  override def isDefinedAt(instruction: Instruction) =
    instruction.inputA >= 0 && instruction.inputA < 4
  override def toString = name

sealed trait IROperation(name: String, calc: (Int, Int) => Int) extends Operation:
  def apply(instruction: Instruction): Register => Register =
    register => register.updated(instruction.output, calc(instruction.inputA, register(instruction.inputB)))
  override def isDefinedAt(instruction: Instruction) =
    instruction.inputB >= 0 && instruction.inputB < 4
  override def toString = name

case object AddR extends RROperation("addr", _ + _)
case object AddI extends RIOperation("addi", _ + _)
case object MulR extends RROperation("mulr", _ * _)
case object MulI extends RIOperation("muli", _ * _)
case object BanR extends RROperation("banr", _ & _)
case object BanI extends RIOperation("bani", _ & _)
case object BorR extends RROperation("borr", _ | _)
case object BorI extends RIOperation("bori", _ | _)

case object GtRR extends RROperation("gtrr", (a, b) => if a > b then 1 else 0)
case object GtRI extends RIOperation("gtri", (a, b) => if a > b then 1 else 0)
case object GtIR extends IROperation("gtir", (a, b) => if a > b then 1 else 0)
case object EqRR extends RROperation("eqrr", (a, b) => if a == b then 1 else 0)
case object EqRI extends RIOperation("eqri", (a, b) => if a == b then 1 else 0)
case object EqIR extends IROperation("eqir", (a, b) => if a == b then 1 else 0)

object Operation:
  val allOperations = Set(AddR, AddI, MulR, MulI, BanR, BanI, BorR, BorI, SetR, SetI, GtRR, GtRI, GtIR, EqRR, EqRI, EqIR)