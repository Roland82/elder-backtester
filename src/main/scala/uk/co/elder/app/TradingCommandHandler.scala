package uk.co.elder.app

import org.joda.time.DateTime
import uk.co.elder.app.model._
import uk.co.elder.app.model.Bid._
import uk.co.elder.app.model.Ticker._
import uk.co.elder.app.model.Volume._

import scalaz.{NonEmptyList, Validation, ValidationNel}
import scalaz.syntax.applicative._
import scalaz.syntax.nel._

sealed trait Command
case class GoLongCommand(date: DateTime, ticker: Ticker, atPrice: Bid, volume: Volume) extends Command
case class SellHolding(date: DateTime, ticker: Ticker, atPrice: Ask, volume: Volume) extends Command

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
        // TODO: Add Volume to this
        val validation =
          Validation.success(cmd.date).toValidationNel[String, DateTime] |@|
          validateTicker(cmd.ticker) |@|
          validateAsk(cmd.atPrice)

        val ownsSharesInTicker = t.tickerSummary(Long).contains(cmd.ticker)

        validation(Sold).ensure("The trader doesn't own shares in this ticker".wrapNel)(_ => ownsSharesInTicker)

      }
    }
  }

  def traderCashValidation(b: Bid, v: Volume, t: Trader): Boolean = {
    val tradeCost = BigDecimal(v.value) * b.value
    tradeCost < t.portfolio.cash
  }
}
