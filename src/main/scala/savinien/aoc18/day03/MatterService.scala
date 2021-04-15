package savinien.aoc18
package day03

import scala.annotation.tailrec

import common.*
import common.pos.Pos

import zio.*

class MatterService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 =
    for
      data      <- input.getData
      claims    <- Claim.fromStringList(data)
      crossover <- MatterService.getMultiClaimCount(claims)
    yield AdventIntResult(crossover)

  override def part2 = 
    for
      data      <- input.getData
      claims    <- Claim.fromStringList(data)
      solitaire <- MatterService.findSolitaireClaim(claims)
    yield AdventIntResult(solitaire.number)

object MatterService:
  private[day03] def getMultiClaimCount(claims: List[Claim]): UIO[Int] =
    @tailrec
    def unionLoop(claim: Claim, others: List[Claim], result: Set[Pos[Int]]): Set[Pos[Int]] = others match
      case Nil          => result
      case head :: tail => 
        if claim.number == head.number then unionLoop(claim, tail, result)
        else claim.area + head.area match
          case None => unionLoop(claim, tail, result)
          case Some(union) =>
            val newResult = result ++ union.cells
            if newResult.size == claim.area.size then newResult
            else unionLoop(claim, tail, newResult)

    @tailrec
    def loop(rest: List[Claim], result: Set[Pos[Int]]): Set[Pos[Int]] = rest match
      case Nil => result
      case head :: tail => 
        val newResult = unionLoop(head, claims, Set.empty) 
        loop(tail, newResult ++ result)

    UIO.succeed(loop(claims, Set.empty).size)

  private[day03] def findSolitaireClaim(claims: List[Claim]): IO[AdventException, Claim] =
    @tailrec
    def isSolitaire(others: List[Claim])(claim: Claim): Boolean = others match
      case Nil          => true
      case head :: tail => 
        if claim.number == head.number then isSolitaire(tail)(claim)
        else claim.area + head.area match
          case None => isSolitaire(tail)(claim)
          case Some(_) => false

    claims.filter(isSolitaire(claims)) match
      case Nil           => IO.fail(NoSolitaireFound)
      case result :: Nil => IO.succeed(result)
      case _             => IO.fail(TooManySolitaireFound)

end MatterService