package savinien.aoc18
package common.geometric

import scala.annotation.targetName
import Turn.*

enum Direction:
  case East, North, West, South

object Direction:
  extension (dir: Direction)
    def turn(turn: Turn): Direction =
      (dir, turn) match
        case (East, Forward) | (North, Right) | (West, Back) | (South, Left) => East
        case (North, Forward) | (West, Right) | (South, Back) | (East, Left) => North
        case (West, Forward) | (South, Right) | (East, Back) | (North, Left) => West
        case (South, Forward) | (East, Right) | (North, Back) | (West, Left) => South

enum Turn:
  case Forward, Left, Right, Back