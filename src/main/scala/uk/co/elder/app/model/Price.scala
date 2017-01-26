package uk.co.elder.app.model;

case class Bid(value: BigDecimal) extends AnyVal
case class Ask(value: BigDecimal) extends AnyVal
case class Mid(value: BigDecimal) extends AnyVal
case class PriceData(ask: Ask, mid: Mid, bid: Bid) {
  def spreadAsPercentage() = ((bid.value - ask.value) / ask.value) * 100

  def spread(): BigDecimal = bid.value - ask.value
}
