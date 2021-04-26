package savinien.aoc18
package day09

import common.MutableRing

class MarbleIterator(winner: Int, counter: Int) extends Iterable[(Int, Int)]:
  override def iterator = new collection.AbstractIterator {
    var ring = MutableRing(0)
    var nextMarble = 1

    def hasNext = true

    def next() =
      var stopMarble = nextMarble + winner - 1
      for marble <- nextMarble until stopMarble do
        ring = ring.rotate(1).append(marble)

      val result = ring.rotate(-counter)
      ring = result.remove

      nextMarble = stopMarble + 1
      (result.value, stopMarble)
  }

object MarbleIterator:
  given defaultIterator: MarbleIterator = new MarbleIterator(23, 7)