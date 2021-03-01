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
      ref <- Ref.make(Map.empty[Pos, ClaimCount])
      _   <- ZIO.foreachPar(claims) {
        claim => 
          ZIO.foreachPar (claim.area.iterator) { pos=>
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
    for
      fabric <- getFabric(claims)
      result <- ZIO.foldLeft (fabric.values) (0) { 
        (count, claims) => 
          if claims == MultiClaim then
            ZIO.succeed(count + 1)
          else
            ZIO.succeed(count)
      }
    yield result

  private[day03] def findAllSingleClaims(fabric: Iterable[ClaimCount]): UIO[Map[Claim, Int]] =
    for
      ref    <- Ref.make(Map.empty[Claim, Int])
      _      <- ZIO.foreachPar (fabric) { 
                  case SingleClaim(claim) =>
                    ref.update {
                      map => map + (claim -> (map.getOrElse(claim, 0) + 1))
                    }
                  case _ => ZIO.succeed(false)
                }
      result <- ref.get
    yield result

  private[day03] def findSolitaireClaim(claims: List[Claim]): IO[AdventException, Claim] =
    for
      fabric    <- getFabric(claims)
      allSingle <- findAllSingleClaims(fabric.values)
      results   <- ZIO.filter (allSingle) { case (claim, count) => ZIO.succeed(claim.area.size == count)}
      resultList = List.from(results)
      result    <- resultList.size match {
                      case 1 => ZIO.succeed(resultList.head._1)
                      case 0 => ZIO.fail(NoSolitaireFound)
                      case n => ZIO.fail(TooManySolitaireFound(n))
                   }
    yield result
      
end MatterService