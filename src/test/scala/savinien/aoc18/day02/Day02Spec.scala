package savinien.aoc18
package day02

import common._

import zio.test._
import zio.test.Assertion._
import zio.test.mock.Expectation._

object Day02Part1Spec extends DefaultRunnableSpec:
  def spec = Tests.part1

object Day02Part2Spec extends DefaultRunnableSpec:
  def spec = Tests.part2

object Tests:
  def all = suite("Day02All")(
    part1, part2
  )

  def part1 =
    suite("Day02Part1")(
        testM("get correct value for password babac") {
        assertM(day02.InventoryService.count_chars("bababc"))(equalTo(Map('a' -> 2, 'b' -> 3, 'c' -> 1)))
      }
      , testM("get two_three babac") {
        assertM(day02.InventoryService.has_two_three("bababc"))(equalTo((true, true)))
      }
      , testM("get correct value for password abcdef") {
        assertM(day02.InventoryService.count_chars("abcdef"))(
            equalTo(Map('a' -> 1, 'b' -> 1, 'c' -> 1, 'd' -> 1, 'e' -> 1, 'f' -> 1))
        )
      }
      , testM("get two_three abcdef") {
        assertM(day02.InventoryService.has_two_three("abcdef"))(equalTo((false, false)))
      }
      , testM("get correct value for password abbcde") {
        assertM(day02.InventoryService.count_chars("abbcde"))(
            equalTo(Map('a' -> 1, 'b' -> 2, 'c' -> 1, 'd' -> 1, 'e' -> 1))
        )
      }
      , testM("get two_three abbcde") {
        assertM(day02.InventoryService.has_two_three("abbcde"))(equalTo((true, false)))
      }
      , testM("get correct value for password abcccd") {
        assertM(day02.InventoryService.count_chars("abcccd"))(
            equalTo(Map('a' -> 1, 'b' -> 1, 'c' -> 3, 'd' -> 1))
        )
      }
      , testM("get two_three abcccd") {
        assertM(day02.InventoryService.has_two_three("abcccd"))(equalTo((false, true)))
      }
      , testM("get count all") {
        assertM
          (day02.InventoryService.count_two_three(
            List("abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab")
          ))
          (equalTo((4, 3)))
      }

    , testM("day02 gets correct number") {
      val input = AdventInputMock.GetData(
          value(List("abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab").mkString("\n"))
      )

      val result = SingleDay.part1.provideLayer(input >>> day02.live)
      assertM(result)(equalTo(AdventIntResult(12)))
    })

  def part2 = 
    suite("Day02Part2")(
      testM("reject equal") {
        assertM
          (day02.InventoryService.checkCommon(
            "abcde", "abcde"
          ))
          (equalTo(None))
      }
      , testM("reject two off") {
        assertM
          (day02.InventoryService.checkCommon(
            "abcde", "axcye"
          ))
          (equalTo(None))
      }
      , testM("accept one off") {
        assertM
          (day02.InventoryService.checkCommon(
            "fghij", "fguij"
          ))
          (equalTo(Some("fgij")))
      }
    , testM("day02 gets correct tickets") {
      val input = AdventInputMock.GetData(
          value(List("abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz").mkString("\n"))
      )
      val result = SingleDay.part2.provideLayer(input >>> day02.live)
      assertM(result)(equalTo(AdventStringResult("fgij")))
      }
  )

end Tests