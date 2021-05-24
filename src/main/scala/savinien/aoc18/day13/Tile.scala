package savinien.aoc18
package day13

import parsers.TokenParsers.*

enum Tile:
  case Horizontal
  case Vertical
  case CurveNE
  case CurveSE
  case Intersection
  case CartEast
  case CartNorth
  case CartWest
  case CartSouth
  case Space
  case Crash

object Tile:
  def fromChar(char: Char): Option[Tile] =
    char match
      case '-'  => Some(Tile.Horizontal)
      case '|'  => Some(Tile.Vertical)
      case '/'  => Some(Tile.CurveNE)
      case '\\' => Some(Tile.CurveSE)
      case '+'  => Some(Tile.Intersection)
      case '>'  => Some(Tile.CartEast)
      case '^'  => Some(Tile.CartNorth)
      case '<'  => Some(Tile.CartWest)
      case 'v'  => Some(Tile.CartSouth)
      case ' '  => Some(Tile.Space)
      case 'X'  => Some(Tile.Crash)
      case _    => None

  def parser: Parser[Tile] = char ^? checkedOption(fromChar)