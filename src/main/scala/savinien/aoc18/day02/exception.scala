package savinien.aoc18
package day02

import common.*

sealed trait InventoryException extends AdventException

case object NoTicketFound extends InventoryException:
  override def toString() = "No valid ticket was found"