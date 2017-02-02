package uk.co.elder.app

import java.util.UUID

import org.joda.time.DateTime
import uk.co.elder.app.model.Trader._
import uk.co.elder.app.model._

import scalaz.{@@, State}

// TODO: Move these elsewhere
case class TradingError(errorMessage: String)

sealed trait Event
case class CreateTrader(id: TraderId, creationDate: DateTime)
case class SoldShort(date: DateTime, ticker: String @@ Ticker, atPrice: BigDecimal @@Ask, numberOfShares: BigInt) extends Event
case class WentLong(date: DateTime, ticker: String @@ Ticker, atPrice: BigDecimal @@ Bid, volume: Volume) extends Event
case class Covered(date: DateTime,ticker: String @@ Ticker, atPrice: BigDecimal @@ Bid) extends Event
case class Sold(date: DateTime,ticker: String @@ Ticker, atPrice: BigDecimal @@ Ask, volume: Volume) extends Event
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
            a <- reduceLongPosition(ticker, volume)
            _ <- amendCash(BigDecimal(volume.value) * soldPrice)
            t <- addTradingEvent(tradingEvent)
          } yield t

          (currentTrader, stateChange.run(currentTrader)._1)

        case WentLong(_, ticker, bid, volume) =>
          val newHolding = currentTrader.findPosition(ticker, Long)
            .flatMap(h => Some(h.copy(numberOfShares = h.numberOfShares + volume.value)))
            .getOrElse(Holding(ticker, Long, volume.value))

          val newHoldings = removeFromHoldings(currentTrader.portfolio.holdings, ticker, Long) :+ newHolding

          val stateChange = for {
              p <- amendPortfolioDetails(newHoldings, -(BigDecimal(volume.value) * bid))
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