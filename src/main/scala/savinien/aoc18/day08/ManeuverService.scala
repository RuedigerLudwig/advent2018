package savinien.aoc18
package day08

import common.*
import zio.*

import parser.TokenParsers.*

class ManeuverService(input: AdventInput.Service) extends SingleDay.Service:
  override def part1 = 
    for
      data <- input.getData
      meta <- ZioParse.parseAllToZio(Node.metaPattern)(data)
    yield AdventIntResult(meta)

  override def part2 = 
    for
      data <- input.getData
      node <- ZioParse.parseAllToZio(Node.nodePattern)(data)
    yield AdventIntResult(node.value)