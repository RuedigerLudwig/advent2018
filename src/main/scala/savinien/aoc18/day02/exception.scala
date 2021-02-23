package savinien.aoc18
package day02

import advent._

sealed trait InventoryException extends AdventException { self =>
  override def toString() = self match
    case NoTicketFound() => "No valid ticket was found"
}
case class NoTicketFound() extends InventoryException
