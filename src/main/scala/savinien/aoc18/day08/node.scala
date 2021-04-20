package savinien.aoc18
package day08

import common.*
import parser.TokenParsers.*

class Node(childNodes: List[Node], meta: List[Int]):
  lazy val value: Int = 
    if childNodes.isEmpty then meta.sum
    else
      meta.collect {
        case meta if meta <= childNodes.length => childNodes(meta - 1).value
      }.sum

object Node:
  def num = unsignedInteger.token
  def node[T](f: (List[T], List[Int]) => T): Parser[T] =
    for
      numChild   <- num
      numMeta    <- num
      childNodes <- node(f).repeat(numChild)
      meta       <- num.repeat(numMeta)
    yield f(childNodes, meta)

  def metaPattern = node[Int] { _.sum + _.sum } <* space
  def nodePattern = node      { Node(_, _)    } <* space