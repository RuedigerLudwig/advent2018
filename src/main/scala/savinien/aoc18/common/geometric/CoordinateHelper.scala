package savinien.aoc18
package common
package geometric

import math.Integral.Implicits.infixIntegralOps

object CoordinateHelper:
  extension (p: Point[Int])
    def vertPath(steps: Int): Iterable[Point[Int]] = 
      (0 until steps by steps.sign).map { s => Point(p.x, p.y + s) }

    def horizPath(steps: Int): Iterable[Point[Int]] = 
      (0 until steps by steps.sign).map { s => Point(p.x + s, p.y) }

    def diagPath(steps: Int, flipVert: Boolean): Iterable[Point[Int]] = 
      if flipVert then 
        (0 until steps by steps.sign).map { s => Point(p.x + s, p.y - s)}
      else 
        (0 until steps by steps.sign).map { s => Point(p.x + s, p.y + s)}

    def diamond(steps: Int): Iterable[Point[Int]] =
      if steps == 0 then List(p)
      else
        p.diagPath(-steps, true) ++
        (p - Point(steps, -steps)).diagPath(-steps, false) ++
        (p - Point(2 * steps, 0)).diagPath(steps, true) ++
        (p - Point(steps, steps)).diagPath(steps, false)


  def manhatten[Int: Integral](p1: Point[Int], p2: Point[Int]): Int = (p1 - p2).absM

  extension (area: Area[Int])
    def perimeter: Iterable[Point[Int]] = 
      if area.height == 1 then
        if area.width == 1 then List(area.bottomLeft)
        else area.bottomLeft.horizPath(area.width)
      else 
        if area.width == 1 then area.bottomLeft.vertPath(area.height)
        else 
          area.bottomLeft.horizPath(area.width - 1) ++
          Point(area.topRight.x, area.bottomLeft.y).vertPath(area.height - 1) ++
          area.topRight.horizPath(1 - area.width) ++
          Point(area.bottomLeft.x, area.topRight.y).vertPath(1 - area.height)