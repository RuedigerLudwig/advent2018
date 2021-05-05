package savinien.aoc18
package day12

import parsers.TokenParsers.*
import common.*

class TreeLine private (val trees: List[Boolean], val startAt: Long, rules: Rules):
  lazy val value: Long =
    trees.zipWithIndex.collect { case (true, index) => index + startAt } .sum

  override def toString = s"($startAt) - ${trees.map(if _ then '#' else '.').mkString}"

  def spread(times: Long): TreeLine =
    def doMore(prevRule: Int, nextLine: List[Boolean]): List[Boolean] =
      val (nextRule, doesGrow) = rules.next(prevRule, false)
      if nextRule == 0 then nextLine
      else doMore(nextRule, doesGrow :: nextLine)

    def oneIteration(line: List[Boolean], prevRule: Int, nextLine: List[Boolean]): List[Boolean] =
      line match
        case Nil => doMore(prevRule, nextLine).dropWhile(!_).reverse
        case tree :: tail =>
          val (nextRule, doesGrow) = rules.next(prevRule, tree)
          oneIteration(tail, nextRule, doesGrow :: nextLine)

    def multiSpread(times: Long, treeLine: List[Boolean], startAt: Long): TreeLine =
      if times <= 0 then new TreeLine(treeLine, startAt, rules)
      else
        val nextLine = oneIteration(treeLine, 0, Nil)
        val startOffset = nextLine.indexOf(true)
        val newTreeLine = nextLine.drop(startOffset)
        val newStartAt = startAt + startOffset - 2
        if newTreeLine == treeLine then new TreeLine(newTreeLine, newStartAt + (newStartAt - startAt) * times - 1, rules)
        else multiSpread(times - 1, newTreeLine, newStartAt)

    multiSpread(times, trees, startAt)

object TreeLine:
  def apply(treeLine: List[Boolean], rules: Rules): TreeLine =
    val startOffset = treeLine.indexOf(true)
    val startTreeLine = treeLine.drop(startOffset)
    new TreeLine(startTreeLine, startOffset, rules)
    
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
  val treeRule = (input <* string("=>").token) ~: tree
  val treeRules = treeRule.lines ^^ { list => Rules(list.toMap) }

  val parser = init ~: (whitespace.* *> treeRules) ^^ { (line, rules) => TreeLine(line, rules) }

  def fromString(input: String) = ZioParse.parseAllToZio(parser)(input)