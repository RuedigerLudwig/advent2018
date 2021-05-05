package savinien.aoc18.day12

case class Rules private (rules: Vector[Boolean]):
  def next(prevValue: Int, tree: Boolean): (Int, Boolean) =
    assert(0 <= prevValue && prevValue < 32)
    val nextValue = (prevValue >> 1) | (if tree then 16 else 0)
    (nextValue, rules(nextValue))

object Rules:
  def apply(list: Map[Int, Boolean]): Rules =
    val rules = (0 until 32).map { list.getOrElse(_, false) }.toVector
    assert(rules(0) == false)
    new Rules(rules)