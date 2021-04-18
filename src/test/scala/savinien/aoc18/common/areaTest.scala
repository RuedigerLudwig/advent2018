package savinien.aoc18
package common

import scala.language.adhocExtensions

import parser.Success
import parser.TokenParsers.*
import point.Point
import area.Area
import area.Parsers.*

import org.scalatest.*
import flatspec.AnyFlatSpec

class AreaTest extends AnyFlatSpec:

  "an area string".should("be parsed correctly") in {
    val input    = "3,2: 5x4"
    val parser   = areaSizeParser
    val expected = Success(Area(Point(3, 2), Point(7, 5)))
    val result   = parse(parser)(input)
    assert(result == expected)
  }

  "the size of an area".should("be calculated correctly") in {
    val area = Area(Point(3, 2), Point(7, 5))
    val expected = 20
    val result = area.size
    assert(result == expected)
  }

  "an area union".should("equal itself") in {
    val area = Area(Point(3, 2), Point(7, 5))
    val expected = Some(area)
    val result = area.union(area)
    assert(result == expected)
  }

  it.should("return an actual crossover") in {
    val area1 = Area(Point(1, 3), Point(4, 6))
    val area2 = Area(Point(3, 1), Point(6, 4))
    val expected = Some(Area(Point(3, 3), Point(4, 4)))
    val result = area1.union(area2)
    assert(result == expected)
  }
  
  it.should("return the same crossover the other way round") in {
    val area1 = Area(Point(1, 3), Point(4, 6))
    val area2 = Area(Point(3, 1), Point(6, 4))
    val expected = Some(Area(Point(3, 3), Point(4, 4)))
    val result = area2.union(area1)
    assert(result == expected)
  }
  
  it.should("be none for unrelated") in {
    val area1 = Area(Point(1, 3), Point(4, 6))
    val area3 = Area(Point(5, 5), Point(6, 6))
    val expected = None
    val result = area1.union(area3)
    assert(result == expected)
  }

  it.should("be none for unrelated 2") in {
    val area2 = Area(Point(3, 1), Point(6, 4))
    val area3 = Area(Point(5, 5), Point(6, 6))
    val expected = None
    val result = area2.union(area3)
    assert(result == expected)
  }

  it.should("find touching areas correctly") in {
    val area2 = Area(Point(3, 1), Point(6, 4))
    val area3 = Area(Point(5, 4), Point(6, 6))
    val expected = Some(Area(Point(5, 4), Point(6, 4)))
    val result = area2.union(area3)
    assert(result == expected)
  }

  "an area".should("be created from a point") in {
    val area = Area(Point(5, 4))
    val expected = Area(Point(5, 4), Point(5, 4))
    assert(area.size == 1)
    assert(area == expected)
    assert(area.contains(Point(5, 4)))
  }

  it.should("be expandable") in {
    val area = Area(Point(5, 4))
    val expected = Area(Point(3, 3), Point(5, 4))
    val result = area + Point(3, 3)
    assert(result.size == 6)
    assert(result == expected)
    assert(result.contains(Point(3, 3)))
    assert(result.contains(Point(3, 4)))
    assert(result.contains(Point(5, 3)))
    assert(result.contains(Point(5, 4)))
  }

  it.should("be expandable in any direction") in {
    val area = Area(Point(5, 4))
    val expected = Area(Point(5, 3), Point(7, 4))
    val result = area + Point(7, 3)
    assert(result.size == 6)
    assert(result == expected)
    assert(result.contains(Point(5, 3)))
    assert(result.contains(Point(5, 4)))
    assert(result.contains(Point(7, 3)))
    assert(result.contains(Point(7, 4)))
  }

  "fromIterable".should("Return None for an empty list") in {
    val input = List.empty[Point[Int]]
    val expected = None
    val result = Area.fromIterable(input)
    assert(result == expected)
  }

  it.should("work with a singleton list") in {
    val input = List(Point(4, 5))
    val expected = Some(Area(Point(4, 5), Point(4, 5)))
    val result = Area.fromIterable(input)
    assert(result == expected)
  }

  it.should("work with a multi point list list") in {
    val input = List(Point(5, 4), Point(7, 3), Point(6, 4))
    val expected = Some(Area(Point(5, 3), Point(7, 4)))
    val result = Area.fromIterable(input)
    assert(result == expected)
  }

  "the center".should("be the center point for an odd sized area") in {
    val area = Area.bySize(Point(3, 4), Point(5, 7)).get
    val expected = Point(5, 7)
    val result = area.center
    assert(result == expected)
  }

  it.should("be closer to the bottom left for an even one") in {
    val area = Area.bySize(Point(3, 4), Point(6, 8)).get
    val expected = Point(5, 7)
    val result = area.center
    assert(result == expected)
  }