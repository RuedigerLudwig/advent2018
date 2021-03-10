package savinien.aoc18
package day04

import common._
import parser.Parsers._
import parser.Conversions.given
import scala.language.implicitConversions
import java.time.{LocalDateTime, DateTimeException}
import zio._
import Types.GuardNum
import AdventParsers._

enum GuardEntry(time: LocalDateTime):
  case WakesUp(time: LocalDateTime) extends GuardEntry(time)
  case FallsAsleep(time: LocalDateTime) extends GuardEntry(time)
  case ShiftStarts(time: LocalDateTime, id: GuardNum) extends GuardEntry(time)

  def getTime = time

object GuardEntry:
  def guardTimeParser = dateTimeParser.surround('[', ']') <~< whiteSpace

  def wakeUpParser = guardTimeParser <~< "wakes up" ^^ { case time => WakesUp(time) }
  def fallsAsleepParser = guardTimeParser <~< "falls asleep" ^^ { case time => FallsAsleep(time) }
  def shiftStartsParser = guardTimeParser ~ unsignedInteger.surround("Guard #", " begins shift") ^^ { 
    case time ~ guard => ShiftStarts(time, GuardNum(guard)) 
  }

  def entryParser = wakeUpParser | fallsAsleepParser | shiftStartsParser

  def fromString(line: String) = ZioParser.parseAllToZio(entryParser)(line)
  def fromStringList(line: String) = ZioParser.parseAllToZio(entryParser.lines)(line)

  given Ordering[GuardEntry] = Ordering.by(_.getTime)
end GuardEntry
