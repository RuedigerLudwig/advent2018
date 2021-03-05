package savinien.aoc18
package day04

import common._
import java.time.LocalDateTime
import Types.GuardNum

sealed trait GuardException extends AdventException

case class NoCorrectDate(y: Int, m: Int, d: Int, h: Int, n: Int) extends GuardException:
  override def toString() = s"Not a read date: $y-$m-$d $h:$n"

case class CantSleepTwice(guard: GuardNum, time: LocalDateTime) extends GuardException:
  override def toString() = s"Guard #$guard sleeps again at $time"

case class CantWakeUpTwice(guard: GuardNum, time: LocalDateTime) extends GuardException:
  override def toString() = s"Guard #$guard wakes again at $time"

case class NotThisShift(guard: GuardNum, shiftStart: LocalDateTime, time: LocalDateTime) extends GuardException:
  override def toString() = s"Guard #$guard has strange Shift: shiftStart: $shiftStart, shift: $time"

case class IncorrectMinuteOrder(guard: GuardNum, fallsAsleep: LocalDateTime, wakesUp: LocalDateTime) extends GuardException:
  override def toString() = s"Guard #$guard has strange Minutes: fallsAsleep: $fallsAsleep, wakesUp: $wakesUp"

case object EndOnPartial extends GuardException:
  override def toString() = "Shift ended while guard was asleep"

case object SleepBeforeShift extends GuardException:
  override def toString() = "Got SleepCycle before Shift starts"

case object MoreThanOneMaxMinute extends GuardException:
  override def toString() = "There is more than one maximum sleep minute"

case object MoreThanOneOptimalGuard extends GuardException:
  override def toString() = "There is more than one optimal Guard"