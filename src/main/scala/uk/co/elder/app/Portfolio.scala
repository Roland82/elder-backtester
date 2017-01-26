package uk.co.elder.app

import uk.co.elder.app.model.Trader._
import uk.co.elder.app.model.{Ask, Bid, Holding, Trader}
import scalaz.State

case class TradingError(errorMessage: String)
case class Ticker(value: String) extends AnyVal
sealed trait Event

case class SoldShort(ticker: Ticker, atPrice: Ask, numberOfShares: BigInt) extends Event
case class WentLong(ticker: Ticker, atPrice: Bid, numberOfShares: BigInt) extends Event
case class Covered(ticker: Ticker, atPrice: Bid) extends Event
case class Sold(ticker: Ticker, atPrice: Ask) extends Event

sealed trait Direction
case object Short extends Direction
case object Long extends Direction


object TradingEvents {

  def runEvents(traderStartState: Trader, events: List[Event]) = {
    val outputEvents = events.foldLeft(List(traderStartState))((acc, ev) => {
      val (_, latestState) = TradingEvents.handleEvent(ev).run(acc.head)
      latestState :: acc
    })

    outputEvents.reverse
  }

  def handleEvent(tradingEvent: Event): State[Trader, Trader] = State[Trader, Trader] {
    currentTrader => {
      tradingEvent match {

        case Sold(ticker, soldPrice) =>
          searchHoldings(currentTrader, ticker, Long) match {
            case Some(holding) => {
              val newCashPosition = currentTrader.portfolio.cash + holding.currentValue(soldPrice)
              val newPortfolio = currentTrader.portfolio.copy(holdings = removeFromHoldings(currentTrader.portfolio.holdings, ticker, Long), cash = newCashPosition)
              (currentTrader, currentTrader.copy(portfolio = newPortfolio))
            }
            case _ => (currentTrader, currentTrader)
          }

        case WentLong(ticker, bid, numShares) => {
          val newHolding = searchHoldings(currentTrader, ticker, Long)
            .flatMap(h => Some(h.copy(numberOfShares = h.numberOfShares + numShares)))
            .getOrElse(Holding(ticker, Long, numShares))

          val newHoldings = removeFromHoldings(currentTrader.portfolio.holdings, ticker, Long) :+ newHolding
          val newTrader = changeCashAndHoldings.mod(t => (t._1 - (BigDecimal(numShares) * bid.value),  newHoldings), currentTrader)
          (currentTrader, newTrader)
        }
      }
    }
  }

  private def searchHoldings(trader: Trader, ticker: Ticker, direction: Direction): Option[Holding] =
    trader.portfolio.holdings.find(h => h.ticker == ticker && h.direction == direction)


  private def removeFromHoldings(holdings: Vector[Holding], ticker: Ticker, direction: Direction): Vector[Holding] =
    holdings.filter(h => !(h.ticker == ticker && h.direction == direction))
}



