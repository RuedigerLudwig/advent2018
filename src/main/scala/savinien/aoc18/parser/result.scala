package savinien.aoc18.parser

enum ParseResult[+A]:
  case Success[+A](value: A, parsed: Int) extends ParseResult[A]
  case Failure(error: ParseError)         extends ParseResult[Nothing]

  def incParsed(plus: Int): ParseResult[A] = this match
    case Success(value, parsed) => Success(value, parsed + plus)
    case _                      => this

  def mapError(f: ParseError => ParseError): ParseResult[A] = this match
    case Failure(fail) => Failure(f(fail))
    case _             => this