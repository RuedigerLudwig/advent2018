package savinien.aoc18.advent

import savinien.aoc18.Day01

object Days {
  def MAX_DAY = 1

  def getDay(day: Int) = day match {
    case 1 => Day01.live
  }
}
