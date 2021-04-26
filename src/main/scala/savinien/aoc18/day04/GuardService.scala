package savinien.aoc18
package day04

import common.*
import zio.*
import java.time.Duration
import Types.{GuardNum, Minute}

class GuardService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 = 
    for
      data      <- input.getData
      entries   <- GuardEntry.fromStringList(data)
      collected <- GuardService.collectEntries(entries)
      guards    <- GuardService.compressGuards(collected)
      opt       <- GuardService.optimalGuard(guards)
      (guard, sleepMap) = opt
      sleep     <- GuardService.sleepingMinute(sleepMap)
      (minute, _) = sleep
    yield AdventNumResult(guard.toInt * minute.toInt)

  override def part2 =
    for
      data      <- input.getData
      entries   <- GuardEntry.fromStringList(data)
      collected <- GuardService.collectEntries(entries)
      guards    <- GuardService.compressGuards(collected)
      opt       <- GuardService.optimalMinute(guards)
      (guard, minute) = opt
    yield AdventNumResult(guard.toInt * minute.toInt)


object GuardService:
  private[day04] def collectEntries(entries: List[GuardEntry]): IO[GuardException, List[Guard]] =
    ZIO.foldLeft(entries.sorted)(List.empty[Guard]) {
      case (list,          GuardEntry.ShiftStarts(time, id)) => 
        Guard.shiftStarts(id, time).map(guard => guard :: list)

      case (guard :: list, GuardEntry.FallsAsleep(time))     => 
        guard.fallAsleep(time).map(guard => guard::list)

      case (guard :: list, GuardEntry.WakesUp(time))         => 
        guard.wakeUp(time).map(guard => guard::list)

      case (Nil, _) =>
        IO.fail(SleepBeforeShift)
    }

  private[day04] def compressGuards(entries: List[Guard]): IO[AdventException, Map[GuardNum, List[FullSleep]]] = 
      ZIO.foldLeft (entries)(Map.empty) {
        case (map, Guard(_, _, Nil)) => UIO.succeed(map)
        case (map, Guard(number, _, list)) =>
          for
            nextList <- ZIO.foldLeft (list)(map.get(number) getOrElse List.empty) {
              case (list, scc @ FullSleep(fa, wu)) => ZIO.succeed(scc :: list)
              case _ => ZIO.fail(EndOnPartial)
            }
          yield map + (number -> nextList)
      }

  private[day04] def sumSleepingTime(list: List[FullSleep]): Long =
    list.map { case FullSleep(fa, wu) => Duration.between(fa, wu).nn.toMinutes() }.sum
   
  private[day04] def optimalGuard(map: Map[GuardNum, List[FullSleep]]): IO[GuardException, (GuardNum, List[FullSleep])] =
      val (_, result) = map.foldLeft((-1L, Option.empty[(GuardNum, List[FullSleep])])) {
        case ((max, keyValue), (number, list)) =>
          val timeAsleep = sumSleepingTime(list)
          if timeAsleep == max then
            (max, None)
          else if timeAsleep > max then
            (timeAsleep, Some(number, list))
          else
            (max, keyValue)
      }
      IO.fromOption(result).mapError(_ => MoreThanOneOptimalGuard)
    
  private[day04] def optimalMinute(map: Map[GuardNum, List[FullSleep]]): IO[GuardException, (GuardNum, Minute)] =
      ZIO.foldLeft(map)((-1, Option.empty[(GuardNum, Minute)])) {
        case ((max, bestMinute), (guard, list)) =>
          sleepingMinute(list).map {
            case (minute, times) =>
              if times > max then
                (times, Some(guard, minute))
              else if times == max then
                (max, None)
              else
                (max, bestMinute)
          }.catchAll(_ => ZIO.succeed((max, bestMinute)))
      }.flatMap {
        case (_, None) => ZIO.fail(MoreThanOneMaxMinute)
        case (_, Some(guard, minute)) => ZIO.succeed((guard, minute))
      }
    
  private[day04] def sleepingMinute(list: List[FullSleep]): IO[GuardException, (Minute, Int)] =
    val minutes = Array.fill(60)(0)
    for FullSleep(fa, wu) <- list do
      for min <- fa.getMinute() until wu.getMinute() do
        minutes(min) = minutes(min) + 1

    val (bestMinute, max) =
      (1 until 60).foldLeft((Option(0), minutes(0))) {
        case ((bestMinute, max), minute) =>
          val curr = minutes(minute)
          if curr > max then      (Option(minute), curr)
          else if curr < max then (bestMinute, max)
          else                    (None, max)
      }

    ZIO
      .fromOption(bestMinute)
      .map(minute => (Minute(minute), max))
      .mapError(_ => MoreThanOneMaxMinute)

end GuardService