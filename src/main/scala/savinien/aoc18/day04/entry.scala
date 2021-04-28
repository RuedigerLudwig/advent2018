package savinien.aoc18
package day04

import common.*
import parsers.TokenParsers.*
import java.time.{LocalDateTime, DateTimeException}
import zio.*
import Types.GuardNum

enum GuardEntry(time: LocalDateTime):
  case WakesUp(time: LocalDateTime) extends GuardEntry(time)
  case FallsAsleep(time: LocalDateTime) extends GuardEntry(time)
  case ShiftStarts(time: LocalDateTime, id: GuardNum) extends GuardEntry(time)

  def getTime = time

object GuardEntry:
  def guardTimeParser: Parser[LocalDateTime] = dateTimeParser.inSquares.token

  def wakeUpParser:      Parser[GuardEntry] = guardTimeParser <* string("wakes up") ^^ { case time => WakesUp(time) }
  def fallsAsleepParser: Parser[GuardEntry] = guardTimeParser <* string("falls asleep") ^^ { case time => FallsAsleep(time) }
  def shiftStartsParser: Parser[GuardEntry] = guardTimeParser ~: unsignedInteger.between(string("Guard #"), string(" begins shift")) ^^ { 
    case (time, guard) => ShiftStarts(time, GuardNum(guard)) 
  }

  def entryParser: Parser[GuardEntry] = wakeUpParser | fallsAsleepParser | shiftStartsParser

  def fromString(input: String) = ZioParse.parseAllToZio(entryParser)(input)
  def fromStringList(input: String) = ZioParse.parseAllToZio(lines(entryParser))(input)

  given Ordering[GuardEntry] = Ordering.by(_.getTime)
end GuardEntry