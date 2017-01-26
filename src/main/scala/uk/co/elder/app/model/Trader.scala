package uk.co.elder.app.model

import uk.co.elder.app.{Direction, Ticker}

import scalaz.Lens.lensu

case class Trader(portfolio: Portfolio)
case class Holding(ticker: Ticker, direction: Direction, numberOfShares: BigInt) {
  def currentValue(pricePerShare: Ask) = BigDecimal(numberOfShares) * pricePerShare.value
}

object Trader {
  private val changePortfolio = lensu[Trader, Portfolio]((t, p) => t.copy(p), e => e.portfolio)
  private val updateCashAndHoldings = lensu[Portfolio, (BigDecimal, Vector[Holding])]((p, c) => p.copy(cash = c._1, holdings = c._2), e => (e.cash, e.holdings))
  val changeCashAndHoldings = changePortfolio andThen updateCashAndHoldings
}
