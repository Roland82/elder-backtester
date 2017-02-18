package uk.co.elder.app.model

import java.util.UUID

import shapeless.tag.@@
import uk.co.elder.app.{Direction, Event, Long}

import scalaz.std.option.optionInstance
import scalaz.syntax.applicative._
import scalaz.Lens.lensu
import scalaz.{IndexedState}

case class TraderId(id: UUID)
case class Trader(t: TraderId, portfolio: Portfolio, eventHistory: List[Event]) {
  def tickerSummary(d: Direction) = this.portfolio.holdings.filter(_.direction == d).map(_.ticker)
  def findPosition(t: String @@ Ticker, d: Direction) : Option[Holding] = this.portfolio.holdings.find(e => e.ticker == t && e.direction == d)
}
case class Holding(ticker: String @@ Ticker, direction: Direction, numberOfShares: BigInt @@ Volume) {
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
          case None => portfolio.filter(e => (e.ticker != t ) || (e.ticker == t && e.direction != d))
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

  private[app] def reduceLongPosition(ticker: String @@ Ticker, v: BigInt @@ Volume, atPrice : BigDecimal @@ Ask): IndexedState[Trader, Trader, _] = {
    def calculateSharesAbleToBeSold(holding: Option[BigInt @@ Volume], sharesToBeSold: Option[BigInt @@ Volume])(implicit ev: Numeric[BigInt]) : BigInt @@ Volume = {
      (holding |@| sharesToBeSold) { ev.min } map(e => Volume(e)) getOrElse Volume(ev.zero)
    }

    def reduceHolding(h: Option[Holding], reduceBy: BigInt @@ Volume): Option[Holding] =
      h flatMap { e => Some(e.copy(numberOfShares = Volume(e.numberOfShares - reduceBy))) }

    for {
        sharesAbleToBeSold <- portfolioHoldingLens(ticker, Long).map(e => calculateSharesAbleToBeSold(e.map(_.numberOfShares), Some(v)))
        _                  <- portfolioHoldingLens(ticker, Long).mods(e => reduceHolding(e, sharesAbleToBeSold))
        addedCash          <- portfolioCashLens.mods(cash => cash + (BigDecimal(sharesAbleToBeSold) * atPrice))
      } yield addedCash
  }

  private[app] def increaseLongPosition(ticker: String @@ Ticker, volume: BigInt @@ Volume, atPrice : BigDecimal @@ Bid): IndexedState[Trader, Trader, _] = {
    def calculateSharesAbleToBeBought(availableCash : BigDecimal)(implicit ev: Numeric[BigInt]) : BigInt @@ Volume = {
      Volume(ev.min(volume, (availableCash / atPrice).abs.toBigInt()))
    }

    def addHolding(v: BigInt @@ Volume)(h: Option[Holding]): Option[Holding] = h match {
      case Some(holding) => Some(holding.copy(numberOfShares = Volume(holding.numberOfShares + volume)))
      case None => Some(Holding(ticker, Long, v))
    }

    for {
      volumeCanBuy <- portfolioCashLens.st.map(calculateSharesAbleToBeBought)
      _            <- portfolioHoldingLens(ticker, Long).mods(addHolding(volumeCanBuy))
      endState     <- portfolioCashLens.mods(cash => cash - (BigDecimal(volumeCanBuy) * atPrice))
    } yield endState
  }

  private[app] def addTradingEvent(event: Event): IndexedState[Trader, Trader, List[Event]] = {
    for {
      a <- tradingHistoryLens.mods(e => e :+ event)
    } yield a
  }
}
