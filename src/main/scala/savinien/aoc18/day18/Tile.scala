package savinien.aoc18
package day18

import parsers.TokenParsers.*

enum Tile derives CanEqual:
  case Open
  case Trees
  case Lumberyard

object Tile:
  def fromChar(char: Char): Option[Tile] =
    char match
      case '.' => Some(Open)
      case '|' => Some(Trees)
      case '#' => Some(Lumberyard)
      case _ => None

  def parser = char ^? checkedOption(Tile.fromChar)