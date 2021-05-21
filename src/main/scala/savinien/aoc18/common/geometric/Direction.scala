package savinien.aoc18
package common.geometric

import annotation.targetName

enum Direction:
  case East, North, West, South

object Direction:
  val Down = South
  val Up = North
  val Left = West
  val Right = East

  def fromChar(char: Char): Option[Direction] =
    char match
      case 'E' | 'e' => Some(Direction.East)
      case 'N' | 'n' => Some(Direction.North)
      case 'W' | 'w' => Some(Direction.West)
      case 'S' | 's' => Some(Direction.South)
      case _ => None

  extension (dir: Direction)
    def short: String = dir match
      case East => "E"
      case North => "N"
      case West => "W"
      case South => "S"

    def turn(turn: Turn): Direction =
      (dir, turn) match
        case (East, Turn.Forward) | (North, Turn.Right) | (West, Turn.Back) | (South, Turn.Left) => East
        case (North, Turn.Forward) | (West, Turn.Right) | (South, Turn.Back) | (East, Turn.Left) => North
        case (West, Turn.Forward) | (South, Turn.Right) | (East, Turn.Back) | (North, Turn.Left) => West
        case (South, Turn.Forward) | (East, Turn.Right) | (North, Turn.Back) | (West, Turn.Left) => South

    @targetName("opDirPointAdd")
    def `+`[T: Integral](point: Point[T]): Point[T] = point + dir

enum Turn:
  case Forward, Left, Right, Back