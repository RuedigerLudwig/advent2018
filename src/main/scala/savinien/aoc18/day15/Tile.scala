package savinien.aoc18
package day15

import parsers.TokenParsers.*
import common.ParticalHelper.checkedOption

enum Tile:
  case Wall
  case Floor
  case Elf
  case Gnome

object Tile:
  def fromChar(char: Char): Option[Tile] =
    char match
      case '#' => Some(Tile.Wall)
      case '.' => Some(Tile.Floor)
      case 'E' => Some(Tile.Elf)
      case 'G' => Some(Tile.Gnome)
      case _   => None

  def parser: Parser[Tile] = char ^? checkedOption(fromChar)