package savinien.aoc18.advent

trait AdventException extends Exception

case class ThrowableException(error: Throwable) extends AdventException:
  override def toString() = f"Error was raised: $error"

case class MultiError(list: List[AdventException]) extends AdventException:
  override def toString() = list.length match
    case 0 => "Empty Error List"
    case 1 => list(0).toString()
    case _ => f"Multiple Errors: $list"