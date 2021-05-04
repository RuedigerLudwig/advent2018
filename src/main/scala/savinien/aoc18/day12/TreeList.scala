package savinien.aoc18
package day12

import parsers.TokenParsers.*
import common.*

case class Instruction(val input: Int, val doesGrow: Boolean):
  self =>
  private var onTrue = self
  private var onFalse = self

  def next(tree: Boolean) = if tree then onTrue else onFalse
  def nextInput(tree: Boolean) = (input >> 1) + (if tree then 16 else 0)

object Instruction:
  private def ensureAll(treeMap: Map[Int, Instruction]): Map[Int, Instruction] =
    (0 until 32).map { number =>
      number -> treeMap.getOrElse(number, Instruction(number, false))
    }.toMap

  def apply(list: List[(Int, Boolean)]): Instruction =
    val treeMap = ensureAll(list.map { (input, doesGrow) => input -> Instruction(input, doesGrow) }.toMap)
    treeMap.foreach { (_, instruction) =>
      instruction.onTrue = treeMap(instruction.nextInput(true))
      instruction.onFalse = treeMap(instruction.nextInput(false))
    }
    treeMap(0)

case class TreeLine(val trees: List[Boolean], val startAt: Long, instruction: Instruction):
  lazy val value: Long =
    trees.zipWithIndex.collect { case (true, index) => index + startAt } .sum

  override def toString = s"($startAt) - ${trees.map(tree => if tree then '#' else '.').mkString}"

  def spread: TreeLine = spreadTimes(1)

  def spreadTimes(times: Long): TreeLine =
    def doMore(times: Int, lastInstruction: Instruction, nextLine: List[Boolean]): List[Boolean] =
      if times <= 0 then nextLine
      else 
        val nextInstruction = lastInstruction.next(false)
        doMore(times - 1, nextInstruction, nextInstruction.doesGrow :: nextLine)

    def oneSpread(line: List[Boolean], lastInstruction: Instruction, nextLine: List[Boolean]): List[Boolean] =
      line match
        case Nil => doMore(5, lastInstruction, nextLine).dropWhile(!_)
        case tree :: tail =>
          val nextInstruction = lastInstruction.next(tree)
          oneSpread(tail, nextInstruction, nextInstruction.doesGrow :: nextLine)

    def manySpread(times: Long, treeLine: List[Boolean], startAt: Long): TreeLine =
      if times <= 0 then TreeLine(treeLine, startAt, instruction)
      else
        val nextLine = oneSpread(treeLine, instruction, Nil).reverse
        val startOffset = nextLine.indexOf(true)
        val cutNextLine = nextLine.drop(startOffset)
        val newStartAt = startAt + startOffset - 2
        if cutNextLine == treeLine then TreeLine(cutNextLine, newStartAt + (newStartAt - startAt) * times - 1, instruction)
        else manySpread(times - 1, cutNextLine, newStartAt)

    manySpread(times, trees, startAt)

object TreeLine:
  private def toNumber(list: List[Boolean]): Int =
    def loop(list: List[Boolean], value: Int, result: Int): Int =
      list match
        case Nil => result
        case true :: tail => loop(tail, value * 2, value + result)
        case false :: tail => loop(tail, value * 2, result)
    loop(list.take(5), 1, 0)

  val tree = char('#').as(true) | char('.').as(false)
  val initialTrees = tree.*
  val init = string("initial state: ") *> initialTrees

  val input = tree.repeat(5) ^^ toNumber
  val treeInstruction = (input <* string("=>").token) ~: tree
  val treeInstructions = treeInstruction.lines ^^ { Instruction(_) }

  val parser = init ~: (whitespace.* *> treeInstructions) ^^ { (line, instruction) => TreeLine(line, 0, instruction) }

  def fromString(input: String) = ZioParse.parseAllToZio(parser)(input)