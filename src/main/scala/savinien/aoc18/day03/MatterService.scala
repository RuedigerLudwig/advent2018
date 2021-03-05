package savinien.aoc18
package day03

import common._
import zio._

sealed trait ClaimCount
case class SingleClaim(claim: Claim) extends ClaimCount
case object MultiClaim extends ClaimCount

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
  /**
   * This could be solved without Ref or Par. But I thought it was a good exercise
   */
  private[day03] def getFabric(claims: List[Claim]): UIO[Map[Pos, ClaimCount]] =
    for 
      ref  <- Ref.make(Map.empty[Pos, ClaimCount])
      _    <- ZIO.foreachPar(claims) { claim => 
                ZIO.foreachPar (claim.area.cells) { pos =>
                  ref.update { map => 
                    map get(pos) match {
                      case None                 => map + (pos -> SingleClaim(claim))
                      case Some(SingleClaim(_)) => map + (pos -> MultiClaim)
                      case Some(MultiClaim)     => map
                    }
                  }
                }
      }
      result <- ref.get
    yield result

  private[day03] def getMultiClaimCount(claims: List[Claim]): UIO[Int] =
    getFabric(claims).map { _.values.foldLeft(0) {
      case (count, MultiClaim) => count + 1
      case (count, _)          => count 
    }}

  private[day03] def findAllSingleClaims(fabric: Iterable[ClaimCount]): UIO[Map[Claim, Int]] =
    for
      ref    <- Ref.make(Map.empty[Claim, Int])
      _      <- ZIO.foreachPar (fabric) { 
                  case SingleClaim(claim) => ref.update {
                                              _.updatedWith(claim) { _.map(_ + 1).orElse(Some(1)) }
                                            }
                  case _                  => ZIO.unit
                }
      result <- ref.get
    yield result

  private[day03] def findSolitaireClaim(claims: List[Claim]): IO[AdventException, Claim] =
    for
      fabric     <- getFabric(claims)
      allSingle  <- findAllSingleClaims(fabric.values)
      results    <- ZIO.filter (allSingle) { case (claim, count) => ZIO.succeed(claim.area.size == count)}
      result     <- 
        if results.isEmpty then
          ZIO.fail(NoSolitaireFound)
        else if !results.tail.isEmpty then
          ZIO.fail(TooManySolitaireFound)
        else
          ZIO.succeed(results.head._1)
    yield result
      
end MatterService