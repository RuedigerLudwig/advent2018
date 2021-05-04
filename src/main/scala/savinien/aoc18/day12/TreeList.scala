package savinien.aoc18
package day12

import parsers.TokenParsers.*
import common.*

case class Instruction(val input: Int, val doesGrow: Boolean):
  private var onTrue = this
  private var onFalse = this

  def next(tree: Boolean): Instruction = if tree then onTrue else onFalse
  private def nextInput(tree: Boolean) = (input >> 1) | (if tree then 0x10 else 0x00)

object Instruction:
  private def ensureAll(treeMap: Map[Int, Instruction]): Map[Int, Instruction] =
    (0 until 32).map { number =>
      number -> treeMap.getOrElse(number, Instruction(number, false))
    }.toMap

  def apply(list: List[(Int, Boolean)]): Instruction =
    val treeMap = ensureAll(list.map { (input, doesGrow) => input -> new Instruction(input, doesGrow) }.toMap)
    treeMap.foreach { (_, instruction) =>
      instruction.onTrue = treeMap(instruction.nextInput(true))
      instruction.onFalse = treeMap(instruction.nextInput(false))
    }
    val zero = treeMap(0)
    assert(zero.doesGrow == false)
    zero

case class TreeLine private (val trees: List[Boolean], val startAt: Long, instruction: Instruction):
  lazy val value: Long =
    trees.zipWithIndex.collect { case (true, index) => index + startAt } .sum

  override def toString = s"($startAt) - ${trees.map(if _ then '#' else '.').mkString}"

  def spread(times: Long): TreeLine =
    def doMore(prevInstruction: Instruction, nextLine: List[Boolean]): List[Boolean] =
      val nextInstruction = prevInstruction.next(false)
      if nextInstruction.input == 0 then nextLine
      else doMore(nextInstruction, nextInstruction.doesGrow :: nextLine)

    def oneIteration(line: List[Boolean], prevInstruction: Instruction, nextLine: List[Boolean]): List[Boolean] =
      line match
        case Nil => doMore(prevInstruction, nextLine).dropWhile(!_).reverse
        case tree :: tail =>
          val nextInstruction = prevInstruction.next(tree)
          oneIteration(tail, nextInstruction, nextInstruction.doesGrow :: nextLine)

    def multiSpread(times: Long, treeLine: List[Boolean], startAt: Long): TreeLine =
      if times <= 0 then new TreeLine(treeLine, startAt, instruction)
      else
        val nextLine = oneIteration(treeLine, instruction, Nil)
        val startOffset = nextLine.indexOf(true)
        val newTreeLine = nextLine.drop(startOffset)
        val newStartAt = startAt + startOffset - 2
        if newTreeLine == treeLine then new TreeLine(newTreeLine, newStartAt + (newStartAt - startAt) * times - 1, instruction)
        else multiSpread(times - 1, newTreeLine, newStartAt)

    multiSpread(times, trees, startAt)

object TreeLine:
  def apply(treeLine: List[Boolean], startAt: Int, instruction: Instruction): TreeLine =
    val startOffset = treeLine.indexOf(true)
    val newTreeLine = treeLine.drop(startOffset)
    val newStartAt = startAt + startOffset
    new TreeLine(newTreeLine, newStartAt, instruction)
    
  private def toNumber(list: List[Boolean]): Int =
    def loop(list: List[Boolean], value: Int, result: Int): Int =
      list match
        case Nil           => result
        case true  :: tail => loop(tail, value * 2, result + value)
        case false :: tail => loop(tail, value * 2, result)
    loop(list, 1, 0)

  val tree = char('#').as(true) | char('.').as(false)
  val initialTrees = tree.*
  val init = string("initial state: ") *> initialTrees

  val input = tree.repeat(5) ^^ toNumber
  val treeInstruction = (input <* string("=>").token) ~: tree
  val treeInstructions = treeInstruction.lines ^^ { Instruction(_) }

  val parser = init ~: (whitespace.* *> treeInstructions) ^^ { (line, instruction) => TreeLine(line, 0, instruction) }

  def fromString(input: String) = ZioParse.parseAllToZio(parser)(input)