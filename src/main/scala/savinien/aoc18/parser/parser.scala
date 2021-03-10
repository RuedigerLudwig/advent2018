package savinien.aoc18
package parser

import scala.annotation.{targetName, tailrec}
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

import ParseResult._

trait Parser[+A]:
  self =>

  def run: Input => ParseResult[A]

  def show: String

  def as[B](b: => B): Parser[B] = Parser.Assign(self, _ => Right(b))

  def all: Parser[A] = self.skipRight(Parsers.endOfInput)

  def flatMap[B](f: A => Parser[B]): Parser[B] = Parser.FlatMap(self, f)

  def ignore: Parser[Unit] = as(())

  def label(msg: String): Parser[A] = Parser.Label(self, msg)

  def lines: Parser[List[A]] = (sep(Parsers.endOfLine).skipRight(Parsers.endOfLine.optional))

  def many: Parser[List[A]] = Parser.Many(self)

  @targetName("opMany")
  def `*`: Parser[List[A]] = many

  def map[B](f: A => B): Parser[B] = flatMap { a => Parser.Succeed(f(a)) }

  def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] = flatMap { a => p2.map(b => f(a, b)) }

  def mapAssign[B](f: A => B): Parser[B] = Parser.Assign(self, a => Right(f(a)))

  @targetName("opMapAssign")
  infix def `^^`[B](f: A => B): Parser[B] = mapAssign(f)

  @targetName("mapAssignEither")
  def mapAssign[B](f: A => Either[String, B]): Parser[B] = Parser.Assign(self, f)

  @targetName("opMapAssignEither")
  infix def `^^`[B](f: A => Either[String, B]): Parser[B] = mapAssign(f)

  def oneOrMany: Parser[List[A]] = map2(*)(_ :: _)

  @targetName("opOneOrMany")
  def `+`: Parser[List[A]] = oneOrMany

  def optional: Parser[Option[A]] = map(Some(_)) | Parser.Succeed(None)

  @targetName("opOptional") 
  def `?`: Parser[Option[A]] = optional

  def or[B >: A](p2: => Parser[B]): Parser[B] = Parser.Or(self, p2)

  @targetName("opOr") 
  infix def `|`[B >: A](p2: => Parser[B]): Parser[B] = or(p2)

  def product[B](p2: => Parser[B]): Parser[Parsers.~[A, B]] = map2(p2)(new Parsers.~(_, _))

  @targetName("opProduct")
  infix def `~`[B](p2: => Parser[B]): Parser[Parsers.~[A, B]] = product(p2)

  def sep(p2: => Parser[Any]): Parser[List[A]] =
    sep1(p2).or(Parser.Succeed(Nil))

  @targetName("opSep")
  def `*`(p2: => Parser[Any]): Parser[List[A]] = sep(p2)

  def sep1(p2: => Parser[Any]): Parser[List[A]] =
    map2((p2 >~> self).many)(_ :: _)

  @targetName("opSep1")
  def `+`(p2: => Parser[Any]): Parser[List[A]] = sep1(p2)

  def scope(msg: String): Parser[A] = Parser.Scope(self, msg)

  def skipRight[B](p2: => Parser[B]): Parser[A] = map2(p2)((a, _) => a)

  @targetName("opSkipRight")
  def `<~<`[B](p2: => Parser[B]): Parser[A] = skipRight(p2)

  def skipLeft[B](p2: => Parser[B]): Parser[B] = map2(p2)((_, b) => b)

  @targetName("opSkipLeft")
  def `>~>`[B](p2: => Parser[B]): Parser[B] = skipLeft(p2)

  def surround(pre: => Parser[Any], post: => Parser[Any]): Parser[A] =
    pre.skipLeft(self.skipRight(post))

  def times(n: Int): Parser[List[A]] = Parser.Times(self, n)

  def trim: Parser[A] = trimLeft.trimRight

  def trimLeft: Parser[A] = Parsers.whiteSpace.skipLeft(self)

  def trimRight: Parser[A] = self.skipRight(Parsers.whiteSpace)

object Parser:
  private[parser] class Succeed[A](a: => A) extends Parser[A]:
    override def run = input => Success(a, 0)
    override def show: String = s"Succeed($a)"

  private[parser] class Fail(error: => ParseError) extends Parser[Nothing]:
    override def run = _ => Failure(error)
    override def show: String = "Fail"

  private[parser] class Assign[A, B](p: Parser[A], f: A => Either[String, B]) extends Parser[B]:
    override def run = input => p.run(input) match
      case fail @ Failure(_) => fail
      case Success(a, n)     => f(a) match
                                  case Right(b)  => Success(b, n)
                                  case Left(msg) => Failure(ParseError(msg, input))
                                  
    override def show: String = s"Assign(${p.show})"

  private[parser] class FlatMap[A, B](p: Parser[A], f: A => Parser[B]) extends Parser[B]:
    override def run = input => p.run(input) match
      case fail @ Failure(_) => fail
      case Success(a, n)     => f(a).run(input.addOffset(n)).incParsed(n)
      
    override def show: String = s"FlatMap(${p.show})"

  private[parser] class Label[A](p: Parser[A], msg: String) extends Parser[A]:
    override def run = input => 
      p.run(input).mapError(_.relabel(msg))
      
    override def show: String = s"Label(${p.show})"

  private[parser] class Many[A](p: Parser[A]) extends Parser[List[A]]:
    @tailrec
    private def loop(input: Input, buf: ListBuffer[A], parsed: Int): ParseResult[List[A]] =
      p.run(input) match
        case Success(a, n) => 
          buf += a
          loop(input.addOffset(n), buf, parsed + n)

        case Failure(_) =>
          Success(buf.toList, parsed)

    override def run = input =>
      loop(input, ListBuffer[A](), 0)
      
    override def show: String = s"Many(${p.show})"

  private[parser] class Or[A, B>: A](p1: Parser[A], p2: => Parser[B]) extends Parser[B]:
    override def run = input => p1.run(input) match
      case Failure(fail) => p2.run(input).mapError(fail2 => fail.merge(fail2))
      case result        => result
      
    override def show: String = s"Or(${p1.show}, ${p2.show})"

  private[parser] class Scope[A](p: Parser[A], msg: String) extends Parser[A]:
    override def run = input => 
      p.run(input).mapError(_.push(msg, input))

    override def show: String = s"Scope(${p.show})"

  private[parser] class StringParser(str: String) extends Parser[String]:
    override def run = input =>
      if input.toParse.startsWith(str) then
        Success(str, str.length)
      else
        Failure(ParseError(s"Expected: $str", input))

    override def show: String = s"StringParser($str)"

  private[parser] class RegexParser(re: Regex) extends Parser[String]:
    override def run = input => 
      re.findPrefixOf(input.toParse) match
        case Some(result) => Success(result, result.length)
        case None         => Failure(ParseError(s"Does not match $re", input))

    override def show: String = s"RegexParser($re)"

  private[parser] class Times[A](p: Parser[A], times: Int) extends Parser[List[A]]:
    @tailrec
    private def loop(input: Input, buf: ListBuffer[A], times: Int, parsed: Int): ParseResult[List[A]] =
      if times <= 0 then
        Success(buf.toList, parsed)
      else
        p.run(input) match
          case Success(a, n) => 
            buf += a
            loop(input.addOffset(n), buf, times - 1, parsed + n)

          case fail @ Failure(_) =>
            fail

    override def run = input =>
      loop(input, ListBuffer[A](), times, 0)

    override def show: String = s"Times(${p.show})"
