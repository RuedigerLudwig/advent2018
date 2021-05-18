package savinien.aoc18.day19

opaque type Register = Vector[Int]

extension (register: Register)
  def get(pos: Int) = register(pos)
  def isDefinedAt(pos: Int) = 0 <= pos && pos < register.length

  def updated(pos: Int, value: Int): Register =
    if !register.isDefinedAt(pos) then register
    else register.updated(pos, value)

  def inc(pos: Int): Register =
    if !register.isDefinedAt(pos) then register
    else register.updated(pos, register(pos) + 1)

  def diff(other: Register): Register =
    register.zip(other).map { (a, b) => a - b }

  def singleIndex: Option[(Int, Int)] =
    register.zipWithIndex.foldLeft ((true, Option.empty[(Int, Int)])) {
      case ((true, factorIndex), (0, _)) => (true, factorIndex)
      case ((true, None), (1, idx))      => (true, Some((1, idx)))
      case ((true, None), (-1, idx))     => (true, Some((-1, idx)))
      case _                             => (false, None)
    }._2

  def show: String = s"[${register.mkString(", ")}]"

object Register:
  def apply(ints: Int*): Register = Vector.from(ints)
  def empty(len: Int): Register = Vector.fill(len)(0)