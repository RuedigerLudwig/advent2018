package savinien.aoc18
package common.geometric

import scala.annotation.targetName

enum Direction:
  case East, North, West, South

object Direction:
  val Down = South
  val Up = North
  val Left = West
  val Right = East

  extension (dir: Direction)
    def turn(turn: Turn): Direction =
      (dir, turn) match
        case (East, Turn.Forward) | (North, Turn.Right) | (West, Turn.Back) | (South, Turn.Left) => East
        case (North, Turn.Forward) | (West, Turn.Right) | (South, Turn.Back) | (East, Turn.Left) => North
        case (West, Turn.Forward) | (South, Turn.Right) | (East, Turn.Back) | (North, Turn.Left) => West
        case (South, Turn.Forward) | (East, Turn.Right) | (North, Turn.Back) | (West, Turn.Left) => South

enum Turn:
  case Forward, Left, Right, Back