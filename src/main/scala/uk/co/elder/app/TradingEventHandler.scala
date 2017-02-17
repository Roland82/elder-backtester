package uk.co.elder.app

import org.joda.time.DateTime
import shapeless.tag
import uk.co.elder.app.model.Trader._
import uk.co.elder.app.model._
import shapeless.tag.@@

import scalaz.State

// TODO: Move these elsewhere
case class TradingError(errorMessage: String)

sealed trait Event
case class  CreateTrader(id: TraderId, creationDate: DateTime)
case class SoldShort(date: DateTime, ticker: String @@ Ticker, atPrice: BigDecimal @@ Ask, numberOfShares: BigInt @@ Volume) extends Event
case class WentLong(date: DateTime, ticker: String @@ Ticker, atPrice: BigDecimal @@ Bid, volume: BigInt @@ Volume) extends Event
case class Covered(date: DateTime,ticker: String @@ Ticker, atPrice: BigDecimal @@ Bid) extends Event
case class Sold(date: DateTime,ticker: String @@ Ticker, atPrice: BigDecimal @@ Ask, volume: BigInt @@ Volume) extends Event
case class DividendPaid(date: DateTime, ticker: String @@ Ticker, amountPaid: BigDecimal) extends Event

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
        case Sold(_, ticker, soldPrice, volume) =>
          val stateChange = for {
            h <- reduceLongPosition(ticker, volume, soldPrice)
            t <- addTradingEvent(tradingEvent)
          } yield t

          (currentTrader, stateChange.run(currentTrader)._1)

        case WentLong(_, ticker, bid, volume) =>
          val newHolding = currentTrader.findPosition(ticker, Long)
            .flatMap(h => Some(h.copy(numberOfShares = tag[Volume](h.numberOfShares + volume.bigInteger))))
            .getOrElse(Holding(ticker, Long, volume))

          val newHoldings = removeFromHoldings(currentTrader.portfolio.holdings, ticker, Long) :+ newHolding

          val stateChange = for {
              p <- amendPortfolioDetails(newHoldings, -(BigDecimal(volume) * bid))
              t <- addTradingEvent(tradingEvent)
            } yield (p, t)

          (currentTrader, stateChange.run(currentTrader)._1)

        case DividendPaid(_, _, amountPaid) => {
          val stateAction = for {
            c <- amendCash(amountPaid)
            t <- addTradingEvent(tradingEvent)
          } yield (c, t)

          (currentTrader, stateAction.run(currentTrader)._1)
        }
      }
    }
  }

  private def removeFromHoldings(holdings: Vector[Holding], ticker: String @@ Ticker, direction: Direction): Vector[Holding] =
    holdings.filter(h => !(h.ticker == ticker && h.direction == direction))
}