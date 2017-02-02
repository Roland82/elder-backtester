import java.util.UUID

import org.joda.time.DateTime
import org.scalatest.{FunSpec, Matchers}
import uk.co.elder.app.{GoLongCommand, Long, SellHolding, Short, Sold, WentLong}
import uk.co.elder.app.TradingCommandHandler.handleCommand
import uk.co.elder.app.model._

import scalaz.{-\/, IList, \/-}

class TradingCommandHandlerSpec extends FunSpec with Matchers {
  val now = DateTime.now()
  private val traderId = TraderId(UUID.randomUUID())

  describe("TradingCommandHandler receiving a sell holding command") {
    val shortHolding = Holding(Ticker("CTAG.L"), Short, 100)
    val longHolding = Holding(Ticker("SGP.L"), Long, 100)
    val trader = Trader(traderId, Portfolio(Vector(shortHolding, longHolding), 1000), List.empty)

    it("should validate all parameters") {
      val command = SellHolding(DateTime.now(), ticker = Ticker(""), atPrice = Ask(0), volume = Volume(0))
      val result = handleCommand(trader, command)
      result.isFailure shouldEqual true
      result.disjunction match {
        case -\/(l) => {
          l.tail.+:(l.head).toList should contain allOf
            (
              "The ticker value is empty",
              "The Ask price must be greater than 0"
            )
        }
        case \/-(_) => fail
      }
    }

    it("should not be able to sell a holding they do not own") {
      val command = SellHolding(DateTime.now(), ticker = Ticker("TSCO.L"), atPrice = Ask(200), volume = Volume(10))
      val result = handleCommand(trader, command)
      result.isFailure shouldEqual true
      result.fold(
        errors => {
          errors.size shouldEqual 1
          errors.head shouldEqual "The trader doesn't own shares in this ticker"
        },
        success => fail
      )
    }


    it("should ignore a Short position in the ticker that they want to sell for") {
      val command = SellHolding(DateTime.now(), ticker = shortHolding.ticker, atPrice = Ask(200), volume = Volume(shortHolding.numberOfShares))
      val result = handleCommand(trader, command)
      result.isFailure shouldEqual true
      result.fold(
        errors => {
          errors.size shouldEqual 1
          errors.head shouldEqual "The trader doesn't own shares in this ticker"
        },
        success => fail
      )
    }

    it("should return a Sold event if all validation is successful") {
      val command = SellHolding(now, ticker = longHolding.ticker, atPrice = Ask(10), volume = Volume(longHolding.numberOfShares))
      val result = handleCommand(trader, command)
      result.fold(
        e => fail(s"Errors occured: $e"),
        r => r shouldEqual Sold(now, command.ticker, command.atPrice, command.volume)
      )
    }

    it("cannot sell more shares in a holding than they actually own") {
      val command = SellHolding(now, ticker = longHolding.ticker, atPrice = Ask(10), volume = Volume(longHolding.numberOfShares + 1))
      val result = handleCommand(trader, command)
      result.fold(
          e => {
            e.size shouldBe 1
            e.head shouldBe "The trader doesn't own 101 shares in SGP.L. Cannot sell this number of shares."
          },
          _ => fail
      )
    }
  }

  describe("TradingCommandHandler receiving a go long command") {
    val trader = Trader(traderId, Portfolio(Vector(), 1000), List.empty)

    it("should validate all parameters coming in") {
      val command = GoLongCommand(DateTime.now(), ticker = Ticker(""), atPrice = Bid(0), volume = Volume(0))
      val result = handleCommand(trader, command)
      result.isFailure shouldEqual true
      result.disjunction match {
        case -\/(l) => {
          l.tail.+:(l.head).toList should contain allOf
            ("Volume cannot be less than or equal to 0 shares",
              "The ticker value is empty",
              "The Bid price must be greater than 0"
            )
        }
        case \/-(_) => fail
      }
    }

    it("should return a WentLong event if validation succeeds") {
      val command = GoLongCommand(now, ticker = Ticker("SGP.L"), atPrice = Bid(1), volume = Volume(1))
      val result = handleCommand(trader, command)
      result.isSuccess shouldEqual true

      result map {
        event => {
          event.isInstanceOf[WentLong] shouldBe true
          val wentLong = event.asInstanceOf[WentLong]
          wentLong.date shouldEqual now
          wentLong.ticker shouldEqual Ticker("SGP.L")
          wentLong.atPrice shouldEqual Bid(1)
          wentLong.volume shouldEqual Volume(1)
        }
      }
    }

    describe("should fail validation") {
      it("when the trader doesn't have enough cash funds to execute the trade") {
        val command = GoLongCommand(now, ticker = Ticker("SGP.L"), atPrice = Bid(100), volume = Volume(11))
        val result = handleCommand(trader, command)
        result.isFailure shouldEqual true
        result.disjunction match {
          case -\/(l) => {
            l.head  shouldEqual "Trader doesn't have enough cash to execute this trade"
            l.tail shouldEqual IList.empty
          }
          case \/-(_) => fail
        }
      }
    }
  }
}