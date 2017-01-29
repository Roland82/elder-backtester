package uk.co.elder.app.model

import scalaz.ValidationNel
import scalaz.syntax.validation._

case class Bid(value: BigDecimal) extends AnyVal
case class Ask(value: BigDecimal) extends AnyVal
case class Mid(value: BigDecimal) extends AnyVal
case class PriceData(ask: Ask, mid: Mid, bid: Bid) {
  def spreadAsPercentage() = ((bid.value - ask.value) / ask.value) * 100

  def spread(): BigDecimal = bid.value - ask.value
}

object Bid {
  def validateBid(b: Bid): ValidationNel[String, Bid] = {
    if (b.value <= 0) "The Bid price must be greater than 0".failureNel else b.successNel
  }

  def validateAsk(b: Ask): ValidationNel[String, Ask] = {
    if (b.value <= 0) "The Ask price must be greater than 0".failureNel else b.successNel
  }
}
