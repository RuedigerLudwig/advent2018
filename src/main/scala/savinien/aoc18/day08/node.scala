package savinien.aoc18
package day08

import common.*
import parser.TokenParsers.*

class Node(childNodes: List[Node], meta: List[Int]):
  val metaSum: Int = childNodes.map { _.metaSum }.sum + meta.sum

  lazy val value: Int = 
    if childNodes.isEmpty then meta.sum
    else
      meta.collect {
        case meta if meta <= childNodes.length => childNodes(meta - 1).value
      }.sum

object Node:
  def num = unsignedInteger.token
  def node: Parser[Node] = 
    for
      numChild   <- num
      numMeta    <- num
      childNodes <- node.repeat(numChild)
      meta       <- num.repeat(numMeta)
    yield Node(childNodes, meta)

  def pattern = node <* space