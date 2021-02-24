package savinien.aoc18
package day02

import advent._

sealed trait InventoryException extends AdventException

case class NoTicketFound() extends InventoryException:
  override def toString() = "No valid ticket was found"
