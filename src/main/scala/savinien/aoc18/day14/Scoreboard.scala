package savinien.aoc18
package day14

import scala.annotation.tailrec
import common.MutableRing

class Scoreboard(initial: List[Int]):
  assert(!initial.isEmpty)
  private var _appender = MutableRing.empty[Int]
  private var _elves = initial.map { value => 
    assert(value >= 0)
    _appender = _appender.append(value)
    _appender
  }
  private var _size = initial.length

  private def doAppend(list: List[Int]): Unit =
    list.foreach { value =>
      _appender = _appender.append(value)
      _size += 1
    }

  def size = _size
  def elves: List[Int] = _elves.map { _.value }
  def toList: List[Int] = _appender.rotate(1).toList

  def createNewRecipes: Scoreboard =
    val nextList = Scoreboard.sumToList(_elves.map { _.value }.sum, Nil)
    if nextList.isEmpty then
      _appender = _appender.append(0)
      _size += 1
    else 
      doAppend(nextList)
    _elves = _elves.map { elf => elf.rotate(elf.value + 1) }
    this

  def findPart1(optimum: Int, resultLength: Int): String =
    @tailrec
    def loop: String =
      if _size >= optimum + resultLength then
        val resultStart = _appender.rotate(optimum - _size)
        resultStart.take(resultLength).mkString
      else
        createNewRecipes
        loop
    loop

  def findPart2(expected: String): Int =
    val expectedList = expected.map { _ - '0' }.reverse

    @tailrec
    def checkForExpected(steps: Int, segment: MutableRing[Int]): Option[Int] =
      if segment.compareBackwards(expectedList) then Some(steps)
      else if steps == 1 then None
      else checkForExpected(steps - 1, segment.rotate(-1))

    @tailrec
    def loop: Int =
      val prevSize = _size
      createNewRecipes

      val added = _size - prevSize
      checkForExpected(added, _appender) match
        case None       => loop
        case Some(toGo) => _size - expectedList.length - (added - toGo)
    loop

object Scoreboard:
  def sumToList(value: Int, result: List[Int]): List[Int] =
    if value <= 0 then result
    else sumToList(value / 10, (value % 10) :: result)