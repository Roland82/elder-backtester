package uk.co.elder.app.model

import java.util.UUID

import uk.co.elder.app.{Direction, Event}

import scalaz.{@@, IndexedState}
import scalaz.Lens.lensu
import uk.co.elder.app.Long
import uk.co.elder.app.BigDecimalExt

case class TraderId(id: UUID)
case class Trader(t: TraderId, portfolio: Portfolio, eventHistory: List[Event]) {
  def tickerSummary(d: Direction) = this.portfolio.holdings.filter(_.direction == d).map(_.ticker)
  def findPosition(t: String @@ Ticker, d: Direction) : Option[Holding] = this.portfolio.holdings.find(e => e.ticker == t && e.direction == d)
}
case class Holding(ticker: String @@ Ticker, direction: Direction, numberOfShares: BigInt) {
  def currentValue(pricePerShare: BigDecimal @@ Ask) = BigDecimal(numberOfShares) * pricePerShare
}

object Trader {
  private val portfolioLens = lensu[Trader, Portfolio]((t, p) => t.copy(portfolio = p), e => e.portfolio)
  private val portfolioCashLens = portfolioLens andThen lensu[Portfolio, BigDecimal]((p, c) => p.copy(cash = c), e => e.cash)
  private val portfolioHoldingsLens = portfolioLens andThen lensu[Portfolio, Vector[Holding]]((p, c) => p.copy(holdings = c), e => e.holdings)
  private val tradingHistoryLens = lensu[Trader, List[Event]]((t, h) => t.copy(eventHistory = h), trader => trader.eventHistory)

  // TODO: Really hate this. There must be a cleaner way to express this filtering on the portfolio
  private val portfolioHoldingLens = (t: String @@ Ticker, d: Direction) => portfolioHoldingsLens andThen
    lensu[Vector[Holding], Option[Holding]](
      (portfolio, holding) => {
        holding match {
          case Some(h) => {
            if (holding.map(_.numberOfShares).getOrElse(BigInt(0)) > 0)
              portfolio.filter(e => (e.ticker != t ) || (e.ticker == t && e.direction != d)) :+ h
            else
              portfolio.filter(e => (e.ticker != t ) || (e.ticker == t && e.direction != d))
          }
          case None => portfolio
        }
      },
      e => e.find(e => e.ticker == t && e.direction == d)
    )

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

  private[app] def reduceLongPosition(t: String @@ Ticker, v: Volume): IndexedState[Trader, Trader, Option[Holding]] = {
    for {
      p <- portfolioHoldingLens(t, Long).mods(e => e.map(e => e.copy(numberOfShares = e.numberOfShares - v.value)))
    } yield p
  }

  private[app] def addTradingEvent(event: Event): IndexedState[Trader, Trader, List[Event]] = {
    for {
      a <- tradingHistoryLens.mods(e => e :+ event)
    } yield a
  }
}
