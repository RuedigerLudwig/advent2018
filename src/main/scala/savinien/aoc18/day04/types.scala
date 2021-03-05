package savinien.aoc18.day04

import scala.annotation.targetName

object Types:
  opaque type Minute = Int
  object Minute:
    def apply(m: Int): Minute = m

  extension (m: Minute)
    @targetName("MinuteInt")
    def toInt: Int = m

  opaque type GuardNum = Int
  object GuardNum:
    def apply(gn: Int): GuardNum = gn

  extension (gn: GuardNum)
    @targetName("GuardInt")
    def toInt: Int = gn
