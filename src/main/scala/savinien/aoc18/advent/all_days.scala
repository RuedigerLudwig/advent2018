package savinien
package aoc18
package advent

import aoc18.day01
import aoc18.day02

object AllDays:
  def MAX_DAY = 4

  def getDay(day: Int) = day match
    case 1 => day01.live
    case 2 => day02.live
    case 3 => day03.live
    case 4 => day04.live