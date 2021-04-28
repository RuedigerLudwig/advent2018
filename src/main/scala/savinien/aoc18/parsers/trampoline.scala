package savinien.aoc18
package parsers

import scala.annotation.tailrec

enum Trampoline[+A]:
  case Done(value: A)
  case More(call: () => Trampoline[A])
  case FlatMap[A, +B](sub: Trampoline[A], cont: A => Trampoline[B]) extends Trampoline[B]

object Trampoline extends Monad[Trampoline]:
  override def pure[A](a: A): Trampoline[A] = More(() => Done(a))

  extension [A](tramp: Trampoline[A])
    override def flatMap[B](f: A => Trampoline[B]): Trampoline[B] =
      tramp match
        case FlatMap(sub, cont) => FlatMap(sub, x => cont(x).flatMap(f))
        case x                  => FlatMap(x, f)

  @tailrec
  final def resume[A](tramp: Trampoline[A]): Either[() => Trampoline[A], A] =
    tramp match
      case Done(a)       => Right(a)
      case More(call)    => Left(call)
      case FlatMap(a, f) =>
        a match
          case Done(a)       => resume(f(a))
          case More(call2)   => Left(() => FlatMap(call2(), f))
          case FlatMap(b, g) => resume(flatMap(b)(x => flatMap(g(x))(f)))

  @tailrec
  final def runT[A](tramp: Trampoline[A]): A =
    resume(tramp) match
      case Right(value) => value
      case Left(more)   => runT(more())
end Trampoline