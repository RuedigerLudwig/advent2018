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
  )

  val MAX_DAY = finished.length
  val MIN_DAY = MAX_DAY

  def getDay(day: Int) = finished(day - 1)