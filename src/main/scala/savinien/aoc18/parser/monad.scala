package savinien.aoc18
package parser

import scala.annotation.targetName

trait Monad[F[+_]]:
  // Abstract
  extension [A](m: F[A])
    def flatMap[B](f: A => F[B]): F[B]

  def pure[A](a: A): F[A]

  // Concrete
  def lift[A, B](f: A => B): F[A] => F[B] = fa => fa.map(f)

  def tapMsg(message: String): F[Unit] = { println(message); pure(()); }

  extension [A](m: F[A])
    def map[B](f: A => B): F[B] = m.flatMap { a => pure(f(a)) }

    def apply[B](ff: F[A => B]): F[B] = m.flatMap { a => ff.map { _(a) }}

  extension [A](m: F[F[A]])
    def flatten: F[A] = m.flatMap { a => a }