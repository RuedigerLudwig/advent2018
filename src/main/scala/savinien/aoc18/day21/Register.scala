package savinien.aoc18.day21

opaque type Register = Vector[Long]

extension (register: Register)
  def get(pos: Int): Long = register(pos)
  def isDefinedAt = register.isDefinedAt

  def updated(pos: Int, value: Long): Register =
    if !isDefinedAt(pos) then register
    else register.updated(pos, value)

  def inc(pos: Int): Register =
    if !isDefinedAt(pos) then register
    else register.updated(pos, register(pos) + 1)

  def diff(other: Register): Register =
    register.zip(other).map { _ - _ }

  def singleIndex: Option[Int] =
    register.zipWithIndex.foldLeft ((true, Option.empty[Int])) {
      case ((true, factorIndex), (0, _)) => (true, factorIndex)
      case ((true, None), (1, idx))      => (true, Some(idx))
      case _                             => (false, None)
    }._2

  def show: String = s"[${register.mkString(", ")}]"

object Register:
  var length = 7

  def apply(ints: Long*): Option[Register] = 
    if ints.length != length then None
    else Some(Vector.from(ints))

  def empty: Register = Vector.fill(length)(0)