package savinien.aoc18
package day21

import parsers.TokenParsers.*
import savinien.aoc18.day16.RROperation

enum Type:
  case Set, Math, Equals, Compare

sealed trait Operation:
  def isDefinedAt(inputA: Int, inputB: Int): Boolean
  def getA(inputA: Int): Register => Long
  def getB(inputB: Int): Register => Long
  def isRegisterA: Boolean = false
  def isRegisterB: Boolean = false
  def name: String
  def opType: Type
  def calc: (Long, Long) => Long
  override def toString = name

sealed trait RROperation(val name: String, val opType: Type, val calc: (Long, Long) => Long) extends Operation:
  override def getA(inputA: Int): Register => Long = _.get(inputA)
  override def getB(inputB: Int): Register => Long = _.get(inputB)
  override def isRegisterA: Boolean = true
  override def isRegisterB: Boolean = true
  override def isDefinedAt(inputA: Int, inputB: Int) =
    inputA >= 0 && inputA < Register.length && inputB >= 0 && inputB < Register.length

sealed trait RIOperation(val name: String, val opType: Type, val calc: (Long, Long) => Long) extends Operation:
  override def getA(inputA: Int): Register => Long = _.get(inputA)
  override def getB(inputB: Int): Register => Long = _ => inputB
  override def isRegisterA: Boolean = true
  override def isDefinedAt(inputA: Int, inputB: Int) =
    inputA >= 0 && inputA < Register.length

sealed trait IROperation(val name: String, val opType: Type, val calc: (Long, Long) => Long) extends Operation:
  override def getA(inputA: Int): Register => Long = _ => inputA
  override def getB(inputB: Int): Register => Long = _.get(inputB)
  override def isRegisterB: Boolean = true
  override def isDefinedAt(inputA: Int, inputB: Int) =
    inputB >= 0 && inputB < Register.length

sealed trait IIOperation(val name: String, val opType: Type, val calc: (Long, Long) => Long) extends Operation:
  override def getA(inputA: Int): Register => Long = _ => inputA
  override def getB(inputB: Int): Register => Long = _ => inputB
  override def isDefinedAt(inputA: Int, inputB: Int) = true

case object SetR extends RIOperation("setr", Type.Set, (a, _) => a)
case object SetI extends IIOperation("seti", Type.Set, (a, _) => a)

case object AddR extends RROperation("addr", Type.Math, _ + _)
case object AddI extends RIOperation("addi", Type.Math, _ + _)
case object MulR extends RROperation("mulr", Type.Math, _ * _)
case object MulI extends RIOperation("muli", Type.Math, _ * _)
case object BanR extends RROperation("banr", Type.Math, _ & _)
case object BanI extends RIOperation("bani", Type.Math, _ & _)
case object BorR extends RROperation("borr", Type.Math, _ | _)
case object BorI extends RIOperation("bori", Type.Math, _ | _)

case object DivR extends RROperation("divr", Type.Math, _ / _)
case object DivI extends RIOperation("divi", Type.Math, _ / _)

case object GtRR extends RROperation("gtrr", Type.Compare, (a, b) => if a > b then 1 else 0)
case object GtRI extends RIOperation("gtri", Type.Compare, (a, b) => if a > b then 1 else 0)
case object GtIR extends IROperation("gtir", Type.Compare, (a, b) => if a > b then 1 else 0)
case object EqRR extends RROperation("eqrr", Type.Equals, (a, b) => if a == b then 1 else 0)
case object EqRI extends RIOperation("eqri", Type.Equals, (a, b) => if a == b then 1 else 0)
case object EqIR extends IROperation("eqir", Type.Equals, (a, b) => if a == b then 1 else 0)

object Operation:
  val allOperations = List(AddR, AddI, MulR, MulI, BanR, BanI, BorR, BorI, SetR, SetI, GtRR, GtRI, GtIR, EqRR, EqRI, EqIR, DivR, DivI)
  def fromString(name: String) = allOperations.find(_.name == name)