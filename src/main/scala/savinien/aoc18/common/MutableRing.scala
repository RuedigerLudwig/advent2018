package savinien.aoc18.common

sealed trait MutableRing[A]:
  def value: A
  def isEmpty: Boolean
  def append(value: A): MutableRing[A]
  def remove: MutableRing[A]
  def rotate(steps: Int): MutableRing[A]
  def toList: List[A]
  def take(steps: Int): List[A]
  def size: Int
  def compare(iter: IterableOnce[A]): Boolean
  def compareBackwards(iter: IterableOnce[A]): Boolean

private case class EmptyRing[A]() extends MutableRing[A]:
  override def value: A = throw IndexOutOfBoundsException("No value in an empty ring")
  override def isEmpty: Boolean = true
  override def append(value: A): MutableRing[A] = Segment(value)
  override def remove: MutableRing[A] = this
  override def rotate(steps: Int): MutableRing[A] = this
  override def take(steps: Int) = Nil
  override def toList = Nil
  override def compare(iter: IterableOnce[A]) = iter.iterator.isEmpty
  override def compareBackwards(iter: IterableOnce[A]) = iter.iterator.isEmpty
  override def size = 0

private case class Segment[A](val value: A) extends MutableRing[A]:
  private var prev: Segment[A] = this
  private var next: Segment[A] = this
 
  override def isEmpty = false

  override def append(value: A): Segment[A] =
    var segment = new Segment(value)
    segment.prev = this
    segment.next = next
    next.prev = segment
    next = segment
    segment

  override def remove: MutableRing[A] =
    if prev == this then EmptyRing()
    else
      prev.next = next
      next.prev = prev
      next

  override def rotate(steps: Int): Segment[A] =
    if steps == 0 then this
    else if steps > 0 then next.rotate(steps - 1)
    else prev.rotate(steps + 1)

  override def toList: List[A] =
    def loop(segment: Segment[A], result: List[A]): List[A] =
      if segment == this then segment.value :: result
      else loop(segment.prev, segment.value :: result)
    loop(prev, Nil)

  override def take(steps: Int): List[A] =
    def loop(segment: Segment[A], result: List[A], steps: Int): List[A] =
      if steps == 0 then result
      else loop(segment.prev, segment.value :: result, steps - 1)

    if steps >= 0 then loop(rotate(steps), Nil, steps)
    else loop(this, Nil, -steps)

  def compare(iter: Iterator[A]): Boolean =
    if iter.isEmpty then true
    else if iter.next() != value then false
    else next.compare(iter)

  override def compare(iter: IterableOnce[A]) = compare(iter.iterator)

  def compareBackwards(iter: Iterator[A]): Boolean =
    if iter.isEmpty then true
    else if iter.next() != value then false
    else prev.compareBackwards(iter)

  override def compareBackwards(iter: IterableOnce[A]) = compareBackwards(iter.iterator)

  override def size: Int =
    def loop(segment: Segment[A], result: Int): Int =
      if segment == this then result
      else loop(segment.prev, result + 1)
    loop(prev, 1)

object MutableRing:
  def empty[A]: MutableRing[A] = EmptyRing()
  def apply[A](value: A): MutableRing[A] = Segment(value)