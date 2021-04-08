package savinien.aoc18
package parser

import Trampoline.*

type TParser = [ST, LT] =>> [A] =>> ParserState[ST, LT] => Trampoline[(Result[A], ParserState[ST, LT])]

trait TrampolineParsers[ST, LT] extends BaseParsers[TParser[ST, LT]]:
  type Parser[+A] = TParser[ST, LT][A]
  extension [A](p1: Parser[A])
    override def flatMap[B](f: A => Parser[B]): Parser[B] = 
      input => More(() => p1(input).flatMap {
        case (Success(a), output) => More(() => f(a)(output))
        case (Failure(msg), _)    => Trampoline.pure(Failure(msg), input)
        case (Error(msg), _)      => Trampoline.pure(Error(msg), input)
        case (Fatal(msg), _)      => Trampoline.pure(Fatal(msg), input)
      })

  override def or[A, AA >: A](p1: Parser[A])(p2: => Parser[AA]): Parser[AA] =
    input => More(() => p1(input).flatMap {
      case (Success(a), output) => Trampoline.pure(Success(a), output)
      case (Failure(_), _)      => More(() => p2(input))
      case (Error(msg), _)      => Trampoline.pure((Error(msg), input))
      case (Fatal(msg), _)      => Trampoline.pure((Fatal(msg), input))
    })

  def commit[A](p1: Parser[A]): Parser[A] =
    input => More(() => p1(input).flatMap {
      case (Success(a), output) => Trampoline.pure((Success(a), output))
      case (Failure(e), _)      => Trampoline.pure((Error(e), input))
      case (Error(e), _)        => Trampoline.pure((Error(e), input))
      case (Fatal(e), _)        => Trampoline.pure((Fatal(e), input))
    })

  def gate[A](p1: Parser[A]): Parser[A] =
    input => More(() => p1(input).flatMap {
      case (Success(a), output) => Trampoline.pure((Success(a), output))
      case (Failure(e), _)      => Trampoline.pure((Failure(e), input))
      case (Error(e), _)        => Trampoline.pure((Failure(e), input))
      case (Fatal(e), _)        => Trampoline.pure((Fatal(e), input))
    })

  override def pure[A](a: A): Parser[A] = { input => Done((Success(a), input)) }

  override def fail(e: ParserError): Parser[Nothing] = { input => Done((Failure(e), input)) }

  def error(e: ParserError): Parser[Any] = { input => Done((Error(e), input)) }

  override def attempt[A](p: Parser[A]): Parser[A] = 
    input => More(() => p(input).flatMap { case (r, _) => Trampoline.pure(r, input) } )

  def tapP(msg: String): Parser[Unit] = 
    input => println(s"Msg: $msg - Input: $input")
    Trampoline.pure((Success(()), input))

end TrampolineParsers