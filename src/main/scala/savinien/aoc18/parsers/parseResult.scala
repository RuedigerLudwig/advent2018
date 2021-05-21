package savinien.aoc18
package parsers

sealed trait Result[+A]
case class Success[A](a: A)                 extends Result[A]
case class Failure(failure: ParserError)    extends Result[Nothing]
case class Error(error: ParserError)        extends Result[Nothing]
case class Fatal(fatal: ParserError)        extends Result[Nothing]

object Result extends Monad[Result]:
  extension [A](result: Result[A])
    override def flatMap[B](f: A => Result[B]): Result[B] =
      result match
        case Success(a)                               => f(a)
        case nos @ (Failure(_) | Error(_) | Fatal(_)) => nos
    def unsafeGet: A =
      result match
        case Success(a)                         => a
        case (Failure(_) | Error(_) | Fatal(_)) => ???

  override def pure[A](a: A): Result[A] = Success(a)

  extension [A](result: Result[A])
    def isSuccess: Boolean =
      result match
        case Success(_) => true
        case _ => false

    def isError: Boolean =
      result match
        case Error(_) => true
        case _ => false