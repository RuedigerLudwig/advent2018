package savinien.aoc18.common

sealed trait AdventResult:
  def asString: String

case class AdventNumResult[T: Integral](value: T) extends AdventResult:
  override def asString = value.toString
  override def toString() = value.toString()

case class AdventStringResult(value: String) extends AdventResult:
  override def asString: String = value
  override def toString() = if value.contains("\n") then "\n" + value else value

case class AdventGenResult[T](value: T) extends AdventResult:
  override def asString: String = value.toString
  override def toString() = 
    val str = asString
    if str.contains("\n") then "\n" + str else str