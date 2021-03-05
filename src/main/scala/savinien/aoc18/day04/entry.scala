package savinien.aoc18
package day04

import common._
import common.ZioParser._
import java.time.{LocalDateTime, DateTimeException}
import scala.util.Try
import zio._
import Types.GuardNum

enum GuardEntry(time: LocalDateTime):
  case WakesUp(time: LocalDateTime) extends GuardEntry(time)
  case FallsAsleep(time: LocalDateTime) extends GuardEntry(time)
  case ShiftStarts(time: LocalDateTime, id: GuardNum) extends GuardEntry(time)

  def getTime = time

object GuardEntry:
  def timeParser =
    (lead("[", unsignedInteger) ~ lead("-", leadingZero) ~ lead("-", leadingZero)) ~ (trail(leadingZero, ":") ~ trail(leadingZero, "]")) >>
    {
      case (year ~ month ~ day) ~ (hour ~ minute) => 
        Try(LocalDateTime.of(year, month, day, hour, minute))
          .fold(_ => failure("Not a correct date"), success)
    }

  def wakeUpParser = timeParser <~ "wakes up" ^^ { case time => WakesUp(time) }
  def fallsAsleepParser = timeParser <~ "falls asleep" ^^ { case time => FallsAsleep(time) }
  def shiftStartsParser = trail(timeParser, "Guard #") ~ trail(unsignedInteger, "begins shift") ^^ { case time ~ guard => ShiftStarts(time, GuardNum(guard)) }

  def entryParser = wakeUpParser | fallsAsleepParser | shiftStartsParser

  def fromString(line: String) = parseAllToZio(entryParser)(line)
  def fromStringList(line: String) = parseAllToZioList(entryParser)(line)

given Ordering[GuardEntry] = Ordering.by(_.getTime)