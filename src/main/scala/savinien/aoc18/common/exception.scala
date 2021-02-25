package savinien.aoc18.common

trait AdventException extends Exception

case object NotImplemented extends AdventException:
  override def toString() = "Not implemented"

case class ThrowableException(error: Throwable) extends AdventException:
  override def toString() = f"Error was raised: $error"

case class ReadError(msg: String) extends AdventException:
  override def toString() = s"Error reading data: $msg"

case class MultiError(list: List[AdventException]) extends AdventException:
  override def toString() = list.length match
    case 0 => "Empty Error List"
    case 1 => list(0).toString()
    case _ => s"Multiple Errors: $list"