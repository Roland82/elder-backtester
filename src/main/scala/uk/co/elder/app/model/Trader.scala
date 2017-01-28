package uk.co.elder.app.model

import uk.co.elder.app.{Direction, Event, Ticker}

import scalaz.{IndexedState, IndexedStateT, Lens}
import scalaz.Lens.lensu

case class Trader(portfolio: Portfolio, eventHistory: List[Event])
case class Holding(ticker: Ticker, direction: Direction, numberOfShares: BigInt) {
  def currentValue(pricePerShare: Ask) = BigDecimal(numberOfShares) * pricePerShare.value
}

object Trader {
  private[model] val portfolioLens = lensu[Trader, Portfolio]((t, p) => t.copy(p), e => e.portfolio)
  private[model] val portfolioCashLens = portfolioLens andThen lensu[Portfolio, BigDecimal]((p, c) => p.copy(cash = c), e => e.cash)
  private[model] val portfolioHoldingsLens = portfolioLens andThen lensu[Portfolio, Vector[Holding]]((p, c) => p.copy(holdings = c), e => e.holdings)
  private[model] val tradingHistoryLens: Lens[Trader, List[Event]] = lensu[Trader, List[Event]]((t, h) => t.copy(eventHistory = h), trader => trader.eventHistory)


  private[app] def amendPortfolioDetails(holdings: Vector[Holding], cashToApply: BigDecimal): IndexedState[Trader, Trader, (_,_)] = {
    for {
      a <- portfolioCashLens.mods(e => e + cashToApply)
      x <- portfolioHoldingsLens.mods(h => holdings)
    } yield (x, a)
  }

  private[app] def addTradingEvent(event: Event): IndexedState[Trader, Trader, List[Event]] = {
    for {
      a <- tradingHistoryLens.mods(e => e :+ event)
    } yield a
  }
}
