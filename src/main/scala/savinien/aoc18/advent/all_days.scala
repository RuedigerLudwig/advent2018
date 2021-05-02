package savinien
package aoc18
package advent

object AllDays:
  val finished = List(
     day01.live, 
     day02.live, 
     day03.live, 
     day04.live, 
     day05.live,
     day06.live,
     day07.live,
     day08.live,
     day09.live,
     day10.live,
     day11.live,
  )

  val MAX_DAY = finished.length
  val MIN_DAY = 1

  def getDay(day: Int) = finished(day - 1)