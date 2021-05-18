package savinien.aoc18
package day19

val registerLen = 6

sealed trait Operation:
  def apply(inputA: Int, inputB: Int, output: Int): Register => Register =
    register => register.updated(output, calc(getA(inputA)(register), getB(inputB)(register)))
  def isDefinedAt(inputA: Int, inputB: Int, output: Int): Boolean
  def getA(inputA: Int): Register => Int
  def getB(inputB: Int): Register => Int
  def name: String
  def calc: (Int, Int) => Int
  override def toString = name

sealed trait RROperation(val name: String, val calc: (Int, Int) => Int) extends Operation:
  override def getA(inputA: Int): Register => Int = _.get(inputA)
  override def getB(inputB: Int): Register => Int = _.get(inputB)
  override def isDefinedAt(inputA: Int, inputB: Int, output: Int) =
    inputA >= 0 && inputA < registerLen && inputB >= 0 && inputB < registerLen

sealed trait RIOperation(val name: String, val calc: (Int, Int) => Int) extends Operation:
  override def getA(inputA: Int): Register => Int = _.get(inputA)
  override def getB(inputB: Int): Register => Int = _ => inputB
  override def isDefinedAt(inputA: Int, inputB: Int, output: Int) =
    inputA >= 0 && inputA < registerLen

sealed trait IROperation(val name: String, val calc: (Int, Int) => Int) extends Operation:
  override def getA(inputA: Int): Register => Int = _ => inputA
  override def getB(inputB: Int): Register => Int = _.get(inputB)
  override def isDefinedAt(inputA: Int, inputB: Int, output: Int) =
    inputB >= 0 && inputB < registerLen

sealed trait IIOperation(val name: String, val calc: (Int, Int) => Int) extends Operation:
  override def getA(inputA: Int): Register => Int = _ => inputA
  override def getB(inputB: Int): Register => Int = _ => inputB
  override def isDefinedAt(inputA: Int, inputB: Int, output: Int) = true

case object SetR extends RIOperation("setr", (a, _) => a)
case object SetI extends IIOperation("seti", (a, _) => a)

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
  val allOperations = List(AddR, AddI, MulR, MulI, BanR, BanI, BorR, BorI, SetR, SetI, GtRR, GtRI, GtIR, EqRR, EqRI, EqIR)
    .map(op => op.name -> op).toMap