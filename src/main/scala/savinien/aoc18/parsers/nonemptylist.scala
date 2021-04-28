package savinien.aoc18
package parsers

import scala.annotation.targetName
import scala.annotation.tailrec

object Nel:
  opaque type NonEmptyList[+A] = List[A]

  extension[A](list: NonEmptyList[A])
    def asMatchable: NonEmptyList[A] & Matchable = list
    def apply(i: Int): A                 = list.apply(i)
    def filter(p: A => Boolean): List[A] = list.filter(p)
    def head: A                          = list.head
    def isEmpty: Boolean                 = false
    def iterator: Iterator[A]            = list.iterator
    def length: Int                      = list.length
    def mkString: String                 = list.mkString
    def mkString(sep: String): String    = list.mkString(sep)
    def reverse: NonEmptyList[A]         = list.reverse
    def tail: List[A]                    = list.tail
    def toList: List[A]                  = list

  extension[A, B](list: NonEmptyList[A])
    def flatMap(f: A => NonEmptyList[B]): NonEmptyList[B] = list.flatMap(f)
    def map(f: A => B): NonEmptyList[B]                   = list.map(f)

  extension[A, AA >: A](elem: AA)
    @targetName("opPrepended")
    def ::(list: NonEmptyList[A]): NonEmptyList[AA] = elem :: list

  extension[A, AA >: A](list: NonEmptyList[A])
    def prepended(elem: AA): NonEmptyList[AA] = elem :: list

    @targetName("concatNel")
    def concat(snd: NonEmptyList[AA]): NonEmptyList[AA] = list ::: snd
    @targetName("opConcatNel")
    def :::(snd: NonEmptyList[AA]): NonEmptyList[AA]    = list ::: snd
 
  object NonEmptyList:
    def apply[A](head: A): NonEmptyList[A]                = head :: Nil
    def apply[A](head: A, tail: A*): NonEmptyList[A]      = head :: tail.toList
    def apply[A](head: A, tail: List[A]): NonEmptyList[A] = head :: tail

    def fromIterable[A](it: Iterable[A]): Option[NonEmptyList[A]] =
      if it.isEmpty then None
      else Some(it.toList)

    def unapply[A](list: NonEmptyList[A]) = new NelUnapplied(list)

  class NelUnapplied[A](nel: NonEmptyList[A]):
    def _1: A            = nel.head
    def _2: List[A]      = nel.tail
    def get              = this
    def isEmpty: Boolean = false

  class NonEmptyListLast[A](nel: NonEmptyList[A]):
    def get: A           = nel.head
    def isEmpty: Boolean = nel.length != 1

  object NonEmptyListLast:
    def unapply[A](nel: NonEmptyList[A]) = new NonEmptyListLast(nel)

  class NonEmptyListLead[A](nel: NonEmptyList[A]):
    def _1: A               = nel.head
    def _2: NonEmptyList[A] = NonEmptyList.fromIterable(nel.tail).get
    def get                 = this
    def isEmpty: Boolean    = nel.length <= 1

  object NonEmptyListLead:
    def unapply[A](nel: NonEmptyList[A]) = new NonEmptyListLead(nel)