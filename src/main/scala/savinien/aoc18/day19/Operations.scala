package savinien.aoc18
package day19

val registerLen = 6

sealed trait Operation:
  def apply(inputA: Int, inputB: Int, output: Int): Register => Register =
    register => register.updated(output, calc(getA(inputA)(register), getB(inputB)(register)))
  def isDefinedAt(inputA: Int, inputB: Int, output: Int): Boolean
  def getA(inputA: Int): Register => Int
  def getB(inputB: Int): Register => Int
  def isCompare: Boolean
  def name: String
  def calc: (Int, Int) => Int
  override def toString = name

sealed trait RROperation(val isCompare: Boolean, val name: String, val calc: (Int, Int) => Int) extends Operation:
  override def getA(inputA: Int): Register => Int = _.get(inputA)
  override def getB(inputB: Int): Register => Int = _.get(inputB)
  override def isDefinedAt(inputA: Int, inputB: Int, output: Int) =
    inputA >= 0 && inputA < registerLen && inputB >= 0 && inputB < registerLen

sealed trait RIOperation(val isCompare: Boolean, val name: String, val calc: (Int, Int) => Int) extends Operation:
  override def getA(inputA: Int): Register => Int = _.get(inputA)
  override def getB(inputB: Int): Register => Int = _ => inputB
  override def isDefinedAt(inputA: Int, inputB: Int, output: Int) =
    inputA >= 0 && inputA < registerLen

sealed trait IROperation(val isCompare: Boolean, val name: String, val calc: (Int, Int) => Int) extends Operation:
  override def getA(inputA: Int): Register => Int = _ => inputA
  override def getB(inputB: Int): Register => Int = _.get(inputB)
  override def isDefinedAt(inputA: Int, inputB: Int, output: Int) =
    inputB >= 0 && inputB < registerLen

sealed trait IIOperation(val isCompare: Boolean, val name: String, val calc: (Int, Int) => Int) extends Operation:
  override def getA(inputA: Int): Register => Int = _ => inputA
  override def getB(inputB: Int): Register => Int = _ => inputB
  override def isDefinedAt(inputA: Int, inputB: Int, output: Int) = true

case object SetR extends RIOperation(false, "setr", (a, _) => a)
case object SetI extends IIOperation(false, "seti", (a, _) => a)

case object AddR extends RROperation(false, "addr", _ + _)
case object AddI extends RIOperation(false, "addi", _ + _)
case object MulR extends RROperation(false, "mulr", _ * _)
case object MulI extends RIOperation(false, "muli", _ * _)
case object BanR extends RROperation(false, "banr", _ & _)
case object BanI extends RIOperation(false, "bani", _ & _)
case object BorR extends RROperation(false, "borr", _ | _)
case object BorI extends RIOperation(false, "bori", _ | _)

case object GtRR extends RROperation(true, "gtrr", (a, b) => if a > b then 1 else 0)
case object GtRI extends RIOperation(true, "gtri", (a, b) => if a > b then 1 else 0)
case object GtIR extends IROperation(true, "gtir", (a, b) => if a > b then 1 else 0)
case object EqRR extends RROperation(true, "eqrr", (a, b) => if a == b then 1 else 0)
case object EqRI extends RIOperation(true, "eqri", (a, b) => if a == b then 1 else 0)
case object EqIR extends IROperation(true, "eqir", (a, b) => if a == b then 1 else 0)

object Operation:
  val allOperations = List(AddR, AddI, MulR, MulI, BanR, BanI, BorR, BorI, SetR, SetI, GtRR, GtRI, GtIR, EqRR, EqRI, EqIR)
    .map(op => op.name -> op).toMap