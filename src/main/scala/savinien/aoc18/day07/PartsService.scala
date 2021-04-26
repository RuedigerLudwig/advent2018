package savinien.aoc18
package day07

import annotation.tailrec

import common.*
import parser.TokenParsers.*

import collection.immutable.SortedMap
import zio.*

class PartsService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 =
    for
      data     <- input.getData
      parts    <- PartsService.parseParts(data)
      orderMap <- PartsService.makeOrderMap(parts)
      result   <- PartsService.getOrder(orderMap)
    yield AdventStringResult(result)

  override def part2 = 
    for
      data     <- input.getData
      workers  <- input.getIntSetting("Workers", 5)
      delay    <- input.getIntSetting("Delay", 60)
      parts    <- PartsService.parseParts(data)
      orderMap <- PartsService.makeOrderMap(parts)
      result   <- PartsService.getSharedOrder(orderMap, workers, delay)
    yield AdventNumResult(result)

object PartsService:
  var pattern = upper.between(string("Step "), string(" must be finished before step ")) ~: (upper <* string(" can begin."))

  private[day07] def parseParts(input: String) =
    ZioParse.parseAllToZio(pattern.lines)(input)

  private[day07] def makeOrderMap(list: List[(Char, Char)]): UIO[SortedMap[Char, Set[Char]]] =
    @tailrec
    def loop(list: List[(Char, Char)], orderMap: SortedMap[Char, Set[Char]]): SortedMap[Char, Set[Char]] = list match
      case Nil => orderMap
      case (before, after) :: tail =>
        loop(tail,
          orderMap.updated(after, orderMap.get(after).map { _ + before }.getOrElse(Set(before)))
            .updated(before, orderMap.get(before).getOrElse(Set.empty))
        )
    UIO.succeed(loop(list, SortedMap.empty))

  private[day07] def popLowest(orderMap: SortedMap[Char, Set[Char]]): Option[(Char, SortedMap[Char, Set[Char]])] =
    orderMap
      .find { (_, set) => set.isEmpty } 
      .map { (char, _) => (char, orderMap.removed(char)) }

  private[day07] def removeAsBefore(orderMap: SortedMap[Char, Set[Char]], toRemove: Char): SortedMap[Char, Set[Char]] =
    orderMap.map { (char, set) => char -> set.excl(toRemove) }

  private[day07] def processLowest(orderMap: SortedMap[Char, Set[Char]]): Option[(Char, SortedMap[Char, Set[Char]])] =
    popLowest(orderMap).map { (char, orderMap) => (char, removeAsBefore(orderMap, char)) }

  private[day07] def getOrder(orderMap: SortedMap[Char, Set[Char]]): AdventTask[String] =
    @tailrec
    def loop(orderMap: SortedMap[Char, Set[Char]], result: String): AdventTask[String] =
      if orderMap.isEmpty then IO.succeed(result)
      else processLowest(orderMap) match
        case None                     => IO.fail(NoLowestFound)
        case Some(char, nextOrderMap) => loop(nextOrderMap, result + char)
    loop(orderMap, "")

  private[day07] def getSharedOrder(orderMap: SortedMap[Char, Set[Char]], workers: Int, delay: Int): AdventTask[Int] = 
    @tailrec
    def pickUp(orderMap: SortedMap[Char, Set[Char]], worker: List[Option[(Int, Char)]], newWorker: List[Option[(Int, Char)]], time: Int):
        (SortedMap[Char, Set[Char]], List[Option[(Int, Char)]]) =
      worker match
        case Nil          => (orderMap, newWorker)
        case None :: tail => popLowest(orderMap) match
          case None                 => (orderMap, newWorker ::: worker)
          case Some(char, orderMap) => 
            val finished = time + delay + (char - 'A' + 1)
            pickUp(orderMap, tail, Some((finished, char)) :: newWorker, time)
        case head :: tail => pickUp(orderMap, tail, head :: newWorker, time)

    @tailrec
    def finishWork(orderMap: SortedMap[Char, Set[Char]], worker: List[Option[(Int, Char)]], newWorker: List[Option[(Int, Char)]], time: Int): 
      (SortedMap[Char, Set[Char]], List[Option[(Int, Char)]]) =
      worker match
        case Nil                                                => (orderMap, newWorker)
        case Some((finished, char)) :: tail if time >= finished => finishWork(removeAsBefore(orderMap, char), tail, None :: newWorker, time)
        case head :: tail                                       => finishWork(orderMap, tail, head :: newWorker, time)

    @tailrec
    def nextTime(worker: List[Option[(Int, Char)]], min: Option[Int]): Option[Int] =
      worker match
        case Nil                         => min
        case None :: tail                => nextTime(tail, min)
        case Some((finished, _)) :: tail => nextTime(tail, min.map(_.min(finished)).orElse(Some(finished)))

    @tailrec
    def loop(orderMap: SortedMap[Char, Set[Char]], worker: List[Option[(Int, Char)]], time: Int): AdventTask[Int] =
      val (newOrderMap,  newWorker)  = finishWork(orderMap, worker, List.empty, time)
      val (newOrderMap2, newWorker2) = pickUp(newOrderMap, newWorker, List.empty, time)
      nextTime(newWorker2, None) match
        case None       => if newOrderMap2.isEmpty then IO.succeed(time) else IO.fail(PrematureFinish)
        case Some(time) => loop(newOrderMap2, newWorker2, time)

    val (newOrderMap, worker) = pickUp(orderMap, List.fill(workers)(None), List.empty, 0)
    nextTime(worker, None) match
      case None       => IO.fail(PrematureFinish)
      case Some(time) => loop(newOrderMap, worker, time)

end PartsService