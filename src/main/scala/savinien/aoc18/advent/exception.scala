package savinien.aoc18.advent

trait AdventException extends Exception{ self => 
  override def toString() = self match
    case ThrowableException(e) => f"Throwable: $e"
}

case class ThrowableException(e: Throwable) extends AdventException
