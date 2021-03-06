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

    def vertPathTo(newY: Int): Iterable[Point[Int]] = 
      if newY >= p.y then vertPath(newY - p.y + 1)
      else vertPath(newY - p.y - 1)

    def horizPathTo(newX: Int): Iterable[Point[Int]] = 
      if newX >= p.x then horizPath(newX - p.x + 1)
      else horizPath(newX - p.x - 1)

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
        if area.width == 1 then List(area.topLeft)
        else area.topLeft.horizPath(area.width)
      else 
        if area.width == 1 then area.topLeft.vertPath(area.height)
        else 
          area.topLeft.horizPath(area.width - 1) ++
          Point(area.bottomRight.x, area.topLeft.y).vertPath(area.height - 1) ++
          area.bottomRight.horizPath(1 - area.width) ++
          Point(area.topLeft.x, area.bottomRight.y).vertPath(1 - area.height)