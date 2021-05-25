package savinien.aoc18.common

import scala.util.Try
import java.time.{LocalDateTime, LocalDate, LocalTime}

object ParticalHelper:
  def checkedTry[A, B](f: (A => B)): PartialFunction[A, B] = new PartialFunction:
    private val cache                   = collection.mutable.Map.empty[A, Try[B]]
    private def check(input: A): Try[B] = cache.getOrElseUpdate(input, Try(f(input)))
    override def apply(input: A): B     = check(input).get
    override def isDefinedAt(input: A)  = check(input).isSuccess

  def checkedOption[A, B](f: (A => Option[B])): PartialFunction[A, B] = new PartialFunction:
    private val cache                      = collection.mutable.Map.empty[A, Option[B]]
    private def check(input: A): Option[B] = cache.getOrElseUpdate(input, f(input))
    override def apply(input: A): B        = check(input).get
    override def isDefinedAt(input: A)     = check(input).isDefined

  def checkedIntegral[T: Integral] = checkedOption[String, T](summon[Integral[T]].parseString)
  def checkedTime = checkedTry[(Int, Int), LocalTime]((h, m) => LocalTime.of(h, m).nn)
  def checkedDate = checkedTry[(Int, Int, Int), LocalDate]((y, m, d) => LocalDate.of(y, m, d).nn)