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

  def sat(desc: String)(pre: Char => Boolean): Parser[Char] = msgFilter(item)(pre, c => s"$c did not match predicate $desc")

  def char(c: Char):   Parser[Char] = sat(s"=='$c'") { _ == c }
  def digit:           Parser[Char] = sat("isDigit") { _.isDigit }
  def lower:           Parser[Char] = sat("isLower") { _.isLower }
  def upper:           Parser[Char] = sat("isUpper") { _.isUpper }
  def whitespace:      Parser[Char] = sat("isWhtespace") { _.isWhitespace }
  def horizontalspace: Parser[Char] = sat("isHSpace") { c => c.isWhitespace && !"\n\r".contains(c) }
  def letter:          Parser[Char] = lower | upper
  def alphanum:        Parser[Char] = letter | digit

  def oneOf(s: String): Parser[Char] = sat(s"oneOf\"($s)\"") {  s.contains(_) }

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