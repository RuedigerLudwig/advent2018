package savinien.aoc18
package common

import parser.Parser
import parser.Parsers._
import parser.Conversions.given
import scala.language.implicitConversions
import scala.util.Try
import java.time.{LocalDateTime, LocalDate, LocalTime}

object AdventParsers:
  def lowerCaseStrings: Parser[String] = raw"[a-z]+".r

  def leadingZero: Parser[Int] = '0'.? >~> unsignedInteger

  def timeParser: Parser[LocalTime] = 
    leadingZero ~ (':' >~> leadingZero) ^^ {
      case hour ~ minute =>
        Try(LocalTime.of(hour, minute).nn)
          .fold(_ => Left(s"Not a correct time $hour $minute"), v => Right(v))
    }

  def dateParser: Parser[LocalDate] = 
    unsignedInteger ~ ('-' >~> leadingZero) ~ ('-' >~> leadingZero) ^^ {
      case year ~ month ~ day =>
        Try(LocalDate.of(year, month, day).nn)
          .fold(_ => Left(s"Not a correct date $year $month $day"), v => Right(v))
    }
  
  def dateTimeParser: Parser[LocalDateTime] = dateParser ~ (whiteSpace >~> timeParser) ^^ {
    case date ~ time => LocalDateTime.of(date, time).nn
  }