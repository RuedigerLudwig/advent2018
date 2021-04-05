package savinien.aoc18.common

trait AdventException extends Exception

case object NotImplemented extends AdventException:
  override def toString() = "Not implemented"

case object Unreachable extends AdventException:
  override def toString() = "This error can never happen!"

case class ThrowableException(error: Throwable) extends AdventException:
  override def toString() = s"Error was raised: $error"

case class ParseFailure(msg: String, input: String) extends AdventException:
  override def toString() = s"Failure reading data: $msg"

case class MultiError(list: List[AdventException]) extends AdventException:
  override def toString() = list.length match
    case 0 => "Empty Error List"
    case 1 => list(0).toString()
    case _ => s"Multiple Errors: $list"