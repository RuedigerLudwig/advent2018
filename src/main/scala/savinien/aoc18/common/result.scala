package savinien.aoc18.common

sealed trait AdventResult

case class AdventIntResult(value: Int)       extends AdventResult:
  override def toString() = value.toString()

case class AdventStringResult(value: String) extends AdventResult:
  override def toString() = value