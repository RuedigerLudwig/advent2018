package savinien.aoc18.day21

class NumberProducer private(val modulo: Int, val target: Int):
  def convert(number: Int): Int =
    def loop(mask: Int, factor: Int, number: Int, result: Int): Int =
      if number == 0 then result
      else if mask % 2 == 0 then loop(mask >> 1, factor << 1, number >> 1, result | ((number % 2) * factor))
      else loop(mask >> 1, factor << 1, number, result)
    loop(modulo, 1, number, target)

object NumberProducer:
  def apply(modulo: Int, target: Int): Option[NumberProducer] =
    if target >= modulo || (modulo & target) != target then None
    else Some(new NumberProducer(modulo, target))