package savinien.aoc18
package day04

import java.time.LocalDateTime
import math.Ordered.given
import zio._
import Types.GuardNum

sealed trait SleepCycle
case class Partial(fallsAsleep: LocalDateTime)                          extends SleepCycle
case class FullSleep(fallsAsleep: LocalDateTime, wakesUp: LocalDateTime) extends SleepCycle

final case class Guard(val number: GuardNum, shiftStart: LocalDateTime, val sleepCycles: List[SleepCycle]):
  def fallAsleep(time: LocalDateTime): IO[GuardException, Guard] =
    sleepCycles match
      case Partial(_) :: _ => ZIO.fail(CantSleepTwice(number, time)) 

      case list            => 
        if time < shiftStart || time.minusMinutes(120).nn > shiftStart then
          IO.fail(NotThisShift(number, shiftStart, time))
        else
          ZIO.succeed(Guard(number, shiftStart, Partial(time) :: list))

  def wakeUp(time: LocalDateTime): IO[GuardException, Guard] =
    sleepCycles match
      case Partial(fa) :: list => 
        if time.getMinute() > fa.getMinute() &&
            time.getHour() == fa.getHour() &&
            time.getDayOfMonth() == fa.getDayOfMonth() &&
            time.getMonth() == fa.getMonth() &&
            time.getYear() == fa.getYear() then
          ZIO.succeed(Guard(number, shiftStart, FullSleep(fa, time) :: list))
        else
          ZIO.fail(IncorrectMinuteOrder(number, fa, time))

      case _ => 
        ZIO.fail(CantWakeUpTwice(number, time)) 

object Guard:
  def shiftStarts(number: GuardNum, time: LocalDateTime): UIO[Guard] =
    UIO.succeed(Guard(number, time, List.empty))