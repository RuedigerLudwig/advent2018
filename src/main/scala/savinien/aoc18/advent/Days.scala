package savinien.aoc18
package advent

import day01.Day01

object Days {
  def MAX_DAY = 1

  def getDay(day: Int) = day match {
    case 1 => Day01.live
  }
}
