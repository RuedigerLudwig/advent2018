package savinien.aoc18.common

case class MutableRing[T](val value: T):
  self =>
  
  private var prev: MutableRing[T] = self
  private var next: MutableRing[T] = self
  
  def append(value: T): MutableRing[T] =
    var segment = new MutableRing(value)
    segment.prev = self
    segment.next = self.next
    self.next.prev = segment
    self.next = segment
    segment

  def remove: MutableRing[T] =
    if self.prev == self then
      self
    else
      self.prev.next = self.next
      self.next.prev = self.prev
      self.next

  def rotate(steps: Int): MutableRing[T] =
    if steps == 0 then self
    else if steps > 0 then self.next.rotate(steps - 1)
    else self.prev.rotate(steps + 1)

  override def toString: String =
    def loop(segment: MutableRing[T], result: String): String =
      if segment == self then result
      else loop(segment.next, s"$result -> ${segment.value}")
    loop(next, s"-> $value")