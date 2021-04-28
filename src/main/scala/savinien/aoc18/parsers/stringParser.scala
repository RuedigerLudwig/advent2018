package savinien.aoc18
package parsers


object StringParsers extends StringParsers

trait StringParsers extends TrampolineParsers[Char, String]:
  def parse[A](p: Parser[A])(word: String): Result[A] = 
    Trampoline.runT(p(StringParserState(word)))._1

  def parseAll[A](p: Parser[A])(word: String): Result[A] =
    val (result, state) = Trampoline.runT(p(StringParserState(word)))
    if state.hasNext then Failure(ParserError(s"Did not parse complete input: $state"))
    else result

  def item: Parser[Char] =
    input => Trampoline.pure(
      input.next
        .map { (a, output) => (Success(a), output) }
        .getOrElse (Failure(ParserError("End of Input reached")), input)
    )

  def items(c: Int): Parser[String] =
    input => Trampoline.pure(
      input.next(c)
        .map { (a, output) => (Success(a), output) }
        .getOrElse (Failure(ParserError("End of Input reached")), input)
    )

  def sat(pre: Char => Boolean): Parser[Char] = filter(item)(pre)

  def char(c: Char):   Parser[Char] = sat { _ == c }
  def digit:           Parser[Char] = sat { _.isDigit }
  def lower:           Parser[Char] = sat { _.isLower }
  def upper:           Parser[Char] = sat { _.isUpper }
  def whitespace:      Parser[Char] = sat { _.isWhitespace }
  def horizontalspace: Parser[Char] = sat { c => c.isWhitespace && !"\n\r".contains(c) }
  def letter:          Parser[Char] = lower | upper
  def alphanum:        Parser[Char] = letter | digit

  def oneOf(s: String): Parser[Char] = sat {  s.contains(_) }

  def endOfLine: Parser[Unit] = (string("\r\n") | (char('\n') | char('\r'))).unit

  def string(str: String): Parser[String] = 
    filter(items(str.length))(_ == str)

  def merge(p: => Parser[Char]): Parser[String] = many1(p).mkString
  def digits: Parser[String] = merge(digit)
  def lowers: Parser[String] = merge(lower)
  def uppers: Parser[String] = merge(upper)
  def word:   Parser[String] = merge(letter)
  def space:  Parser[Unit]   = whitespace.*.unit
  def hspace: Parser[Unit]   = horizontalspace.*.unit
end StringParsers