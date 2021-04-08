package savinien.aoc18
package parser

import scala.annotation.targetName
import Nel.*

trait BaseParsers[P[+_]] extends Monad[P]:
  /**
   * abstract
   */
  def or[A, AA >: A](p1: P[A])(p2: => P[AA]): P[AA]
  def fail(e: ParserError): P[Nothing]
  def attempt[A](p: P[A]): P[A]

  /**
   * concrete
   */
  def oneOf[A](ps: NonEmptyList[P[A]]): P[A] = 
    (ps.asMatchable: @unchecked) match
      case NonEmptyListLast(p)       => p
      case NonEmptyListLead(p, rest) => p | oneOf(rest)

  extension [A](p1: P[A])
    def report(msg: String): P[A] = p1.map { a => println(s"Msg: $msg; a: $a"); a }

    def filter(predicate: A => Boolean): P[A] =
      p1.flatMap { a => if predicate(a) then pure(a) else fail(ParserError("Filter did not match")) }

    def withFilter: (A => Boolean) => P[A] = filter

    def as[B](b: B): P[B] = p1.flatMap { _ => pure(b) }

    def unit: P[Unit] = p1.as(())

    def partialMap[B](f: PartialFunction[A, B]): P[B] =
      for
        a <- p1
        if f.isDefinedAt(a)
      yield f(a)

    def eitherMap[B](f: A => Either[ParserError, B]): P[B] = p1.flatMap {
      f(_) match
        case Left(e) => fail(e)
        case Right(b) => pure(b)
    }

    def zipWith[B, C](p2: => P[B])(f: (A, B) => C): P[C] =
      for
        a <- p1
        b <- p2
      yield f(a, b)

    def foldLeft[B](zero: B, f: (B, A) => B): P[B] = 
      (for
        a <- p1
        b <- foldLeft(f(zero, a), f)
      yield b) | pure(zero)

    def foldRight[B](zero: B, f: (A, B) => B): P[B] =
      (for
        a <- p1
        b <- foldRight(zero, f)
      yield f(a, b)) | pure(zero)

    def zip[B](p2: => P[B]): P[(A, B)] = zipWith(p2) { (_, _) }

    def prepended[B <: Tuple](p2: => P[B]): P[A *: B] = zipWith(p2) { _ *: _ }

    def skipLeft[B](p2: => P[B]): P[B] = zipWith(p2) { (_, b) => b }

    def skipRight(p2: => P[Any]): P[A] = zipWith(p2) { (a, _) => a }



    def many: P[List[A]] = zipWith(p1.many) { _ :: _} | pure(Nil)

    def many1: P[NonEmptyList[A]] = zipWith(p1.many) { NonEmptyList(_, _) }

    def optional: P[Option[A]] = p1.map(Some(_)) | pure(None)

    def lazyOpt[B](p2: => P[B]): P[(Option[A], B)] =
      p2.map(b => (None, b)) | zipWith(p2) { (a, b) => (Some(a), b) }

    def lazyMany[B](p2: => P[B]): P[(List[A], B)] =
      def loop(list: List[A]): P[List[A]] =
        attempt(p2).map(_ => list.reverse) | (
          for
            a  <- p1
            as <- loop(a :: list)
          yield as
        )
      loop(Nil) ~: p2

    def lazyMany1[B](p2: => P[B]): P[(NonEmptyList[A], B)] =
      p1 ~: p1.lazyMany(p2) ^^ { (a, as, b)  => (NonEmptyList(a, as), b ) }

    def sepby(s: P[Any]): P[List[A]] =
      p1.sepby1(s).map(_.toList) | pure(Nil)

    def sepby1(s: P[Any]): P[NonEmptyList[A]] =
      zipWith(s.skipLeft(p1).*) { NonEmptyList(_, _) }

    def bracket(open: => P[Any], close: => P[Any]): P[A] =
      for
        _ <- open
        a <- p1
        _ <- close
      yield a

    def repeat(n: Int): P[List[A]] =
      if n <= 0 then pure(Nil)
      else zipWith(repeat(n-1)) { _ :: _ }

    def repeatMax(n: Int): P[List[A]] =
      if n <= 0 then pure(Nil)
      else zipWith(p1.repeatMax(n - 1)) { _ :: _ } | pure(Nil)

    def repeatBetween(min: Int)(max: Int): P[List[A]] =
      repeat(min).zipWith(repeatMax(max - min)) { _ ::: _ }

    @targetName("opMany")
    def `*`: P[List[A]] = p1.many

    @targetName("opMany1")
    def `+`: P[NonEmptyList[A]] = p1.many1

    @targetName("opOptional")
    def `?`: P[Option[A]] = p1.optional

  extension [A](p: P[List[A]])
    @targetName("mkListString")
    def mkString: P[String] = p.map(_.mkString)

    @targetName("mkListSepString")
    def mkString(sep: String): P[String] = p.map(_.mkString(sep))

  extension [A](p: P[NonEmptyList[A]])
    @targetName("mkNelString")
    def mkString: P[String] = p.map(_.mkString)

    @targetName("mkNelSepString")
    def mkString(sep: String): P[String] = p.map(_.mkString(sep))

  extension [A <: Tuple](p: P[A])
    @targetName("mkTupleString")
    def mkString: P[String] = p.map(_.toList.mkString)

    @targetName("mkTupleSepString")
    def mkString(sep: String): P[String] = p.map(_.toList.mkString(sep))

  extension [A, B](p1: P[A])
    @targetName("opMap")
    def `^^`(f: A => B): P[B] = p1.map(f)

    @targetName("opMapPartial")
    def `^?`(f: PartialFunction[A, B]): P[B] = p1.partialMap(f)

    @targetName("opMapEither")
    def `^??`(f: A => Either[ParserError, B]): P[B] = p1.eitherMap(f)

    @targetName("opProduct")
    def `~:`(p2: => P[B]): P[(A, B)] = p1.zip(p2)

    @targetName("opSkipLeft")
    def `*>`(p2: => P[B]): P[B] = p1.skipLeft(p2)

    @targetName("opSkipRight")
    def `<*`(p2: => P[B]): P[A] = p1.skipRight(p2)

    @targetName("opLazyMany")
    def `~*:`(p2: => P[B]): P[(List[A], B)] = p1.lazyMany(p2)

    @targetName("opLazyMany1")
    def `~+:`(p2: => P[B]): P[(NonEmptyList[A], B)] = p1.lazyMany1(p2)

    @targetName("opLazyOpt")
    def `~?:`(p2: => P[B]): P[(Option[A], B)] = p1.lazyOpt(p2)

  extension [A, AA >: A](p1: P[A])
    @targetName("opOr")
    def `|`(p2: => P[AA]): P[AA] = or(p1)(p2)

  extension [A, B <: Tuple](p1: P[A])
    @targetName("opPrepend")
    def `~:`(p2: => P[B]): P[A *: B] = p1.prepended(p2)

end BaseParsers