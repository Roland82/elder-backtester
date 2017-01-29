package uk.co.elder.app.model

import uk.co.elder.app.{Direction, Event}

import scalaz.IndexedState
import scalaz.Lens.lensu

case class Trader(portfolio: Portfolio, eventHistory: List[Event]) {
  def tickerSummary(d: Direction) = this.portfolio.holdings.filter(_.direction == d).map(_.ticker)
}
case class Holding(ticker: Ticker, direction: Direction, numberOfShares: BigInt) {
  def currentValue(pricePerShare: Ask) = BigDecimal(numberOfShares) * pricePerShare.value
}

object Trader {
  private val portfolioLens = lensu[Trader, Portfolio]((t, p) => t.copy(p), e => e.portfolio)
  private val portfolioCashLens = portfolioLens andThen lensu[Portfolio, BigDecimal]((p, c) => p.copy(cash = c), e => e.cash)
  private val portfolioHoldingsLens = portfolioLens andThen lensu[Portfolio, Vector[Holding]]((p, c) => p.copy(holdings = c), e => e.holdings)
  private val tradingHistoryLens = lensu[Trader, List[Event]]((t, h) => t.copy(eventHistory = h), trader => trader.eventHistory)

  private[app] def amendPortfolioDetails(holdings: Vector[Holding], cashToApply: BigDecimal): IndexedState[Trader, Trader, (_,_)] = {
    for {
      a <- portfolioCashLens.mods(e => e + cashToApply)
      x <- portfolioHoldingsLens.mods(h => holdings)
    } yield (x, a)
  }

  private[app] def amendCash(cashToApply: BigDecimal): IndexedState[Trader, Trader, (_)] = {
    for {
      a <- portfolioCashLens.mods(e => e + cashToApply)
    } yield a
  }

  private[app] def addTradingEvent(event: Event): IndexedState[Trader, Trader, List[Event]] = {
    for {
      a <- tradingHistoryLens.mods(e => e :+ event)
    } yield a
  }
}
