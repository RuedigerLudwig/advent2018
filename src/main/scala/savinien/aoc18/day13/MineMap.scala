package savinien.aoc18
package day13

import common.*
import common.geometric.{Point, Direction, Turn}
import parsers.TokenParsers.*

import collection.immutable.TreeSet

import Cart.given

class MineMap private(val tileMap: Map[Point[Int], Tile], val carts: TreeSet[Cart], val crashSites: List[Point[Int]]):
  override def toString = s"${carts}"
  def moveCart(cart: Cart): Cart = 
    tileMap(cart.nextPos) match
      case Tile.Horizontal | Tile.Vertical => Cart(cart.nextPos, cart.facing, cart.nextTurn)
      case Tile.CurveNE =>
        cart.facing match
          case Direction.East  => Cart(cart.nextPos, Direction.North, cart.nextTurn)
          case Direction.North => Cart(cart.nextPos, Direction.East,  cart.nextTurn)
          case Direction.West  => Cart(cart.nextPos, Direction.South, cart.nextTurn)
          case Direction.South => Cart(cart.nextPos, Direction.West,  cart.nextTurn)
      case Tile.CurveSE =>
        cart.facing match
          case Direction.East  => Cart(cart.nextPos, Direction.South, cart.nextTurn)
          case Direction.South => Cart(cart.nextPos, Direction.East,  cart.nextTurn)
          case Direction.West  => Cart(cart.nextPos, Direction.North, cart.nextTurn)
          case Direction.North => Cart(cart.nextPos, Direction.West,  cart.nextTurn)
      case Tile.Intersection =>
        cart.nextTurn match
          case Turn.Left    => Cart(cart.nextPos, cart.facing.turn(Turn.Left),  Turn.Forward)
          case Turn.Forward => Cart(cart.nextPos, cart.facing,                  Turn.Right)
          case Turn.Right   => Cart(cart.nextPos, cart.facing.turn(Turn.Right), Turn.Left)
          case Turn.Back => ???
      case _ => ???

  def isCrash(cart: Cart, others: TreeSet[Cart]): Boolean = 
    others.exists(_.position == cart.position)

  def oneTick: MineMap =
    def loop(carts: TreeSet[Cart], movedCarts: TreeSet[Cart], crashSites: List[Point[Int]]): MineMap =
      if carts.isEmpty then new MineMap(tileMap, movedCarts, crashSites)
      else
        val current = carts.head
        val rest = carts.tail
        val moved = moveCart(current)
        if isCrash(moved, rest) then
          loop(rest.filter(_.position != moved.position), movedCarts, moved.position :: crashSites)
        else if isCrash(moved, movedCarts) then
          loop(rest, movedCarts.filter(_.position != moved.position), moved.position :: crashSites)
        else loop(rest, movedCarts + moved, crashSites)
    loop(carts, TreeSet.empty, Nil)

object MineMap:
  def apply(rawMap: List[List[Tile]]): MineMap =
    val tileMap = rawMap.zipWithIndex.map { (line, row) =>
      line.zipWithIndex.collect { 
        case (Tile.Horizontal, col) => Point(col, row) -> Tile.Horizontal
        case (Tile.CartEast, col) => Point(col, row) -> Tile.Horizontal
        case (Tile.CartWest, col) => Point(col, row) -> Tile.Horizontal
        case (Tile.Vertical, col) => Point(col, row) -> Tile.Vertical
        case (Tile.CartNorth, col) => Point(col, row) -> Tile.Vertical
        case (Tile.CartSouth, col) => Point(col, row) -> Tile.Vertical
        case (Tile.Intersection, col) => Point(col, row) -> Tile.Intersection
        case (Tile.CurveNE, col) => Point(col, row) -> Tile.CurveNE
        case (Tile.CurveSE, col) => Point(col, row) -> Tile.CurveSE
      }
    }.flatten.toMap
    val carts = TreeSet.from(rawMap.zipWithIndex.map { (line, row) =>
      line.zipWithIndex.collect { 
        case (Tile.CartEast, col) => Cart(Point(col, row), Direction.East, Turn.Left)
        case (Tile.CartWest, col) => Cart(Point(col, row), Direction.West, Turn.Left)
        case (Tile.CartNorth, col) => Cart(Point(col, row), Direction.North, Turn.Left)
        case (Tile.CartSouth, col) => Cart(Point(col, row), Direction.South, Turn.Left)
      }
    }.flatten)
    new MineMap(tileMap, carts, Nil)

  var parser = Tile.parser.*.lines ^^ MineMap.apply

  def fromString(input: String) = ZioParse.parseAllToZio(parser)(input)