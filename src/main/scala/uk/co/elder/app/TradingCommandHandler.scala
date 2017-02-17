package uk.co.elder.app

import org.joda.time.DateTime
import uk.co.elder.app.model.Bid._
import uk.co.elder.app.model.Ask._
import uk.co.elder.app.model.Ticker._
import uk.co.elder.app.model.Volume._
import uk.co.elder.app.model._

import scalaz.syntax.applicative._
import scalaz.syntax.nel._
import scalaz.{Validation, ValidationNel}
import shapeless.tag.@@

sealed trait Command
case class GoLongCommand(date: DateTime, ticker: String @@ Ticker, atPrice: BigDecimal @@ Bid, volume: BigInt @@ Volume) extends Command
case class SellHolding(date: DateTime, ticker: String @@ Ticker, atPrice: BigDecimal @@ Ask, volume: BigInt @@ Volume) extends Command

object TradingCommandHandler {

  def handleCommand(t: Trader, command: Command): ValidationNel[String, Event] = {
    command match {
      case cmd: GoLongCommand => {
        val validation =
          Validation.success(cmd.date).toValidationNel[String, DateTime] |@|
          validateTicker(cmd.ticker) |@|
          validateBid(cmd.atPrice) |@|
          validateVolume(cmd.volume)

        validation(WentLong).ensure("Trader doesn't have enough cash to execute this trade".wrapNel)(e => traderCashValidation(e.atPrice, e.volume, t))
      }

      case cmd: SellHolding => {
        val validation =
          Validation.success(cmd.date).toValidationNel[String, DateTime] |@|
          validateTicker(cmd.ticker) |@|
          validateAsk(cmd.atPrice) |@|
          validateVolume(cmd.volume)

        // TODO: OwnsSharesInTicker could be represented by hasEnoughSharesToSell to save some extra computation
        val ownsSharesInTicker = t.tickerSummary(Long).contains(cmd.ticker)
        val hasEnoughSharesToSell = t.findPosition(cmd.ticker, Long).map(_.numberOfShares).getOrElse(BigInt(0)) >= cmd.volume

        validation(Sold)
          .ensure("The trader doesn't own shares in this ticker".wrapNel)(_ => ownsSharesInTicker)
          .ensure(s"The trader doesn't own ${cmd.volume} shares in ${cmd.ticker}. Cannot sell this number of shares.".wrapNel)(_ => hasEnoughSharesToSell)

      }
    }
  }

  // TODO: Get rid of this
  def traderCashValidation(b: BigDecimal @@ Bid, v: BigInt @@ Volume, t: Trader): Boolean = {
    val tradeCost = BigDecimal(v) * b
    tradeCost < t.portfolio.cash
  }
}
