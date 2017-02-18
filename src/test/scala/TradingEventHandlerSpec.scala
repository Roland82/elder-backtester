import java.util.UUID

import org.joda.time.DateTime
import org.scalatest._
import uk.co.elder.app.model._
import uk.co.elder.app.{DividendPaid, Long, Short, Sold, TradingEvents, WentLong}

class TradingEventHandlerSpec extends FunSpec with Matchers {
  private def poundsAsPence(pounds: Int) = pounds * 100
  private val date = DateTime.now()
  private val traderId = TraderId(UUID.randomUUID())

  describe("Trader who has 100 shares both Long and Short in SGP.L and has some long CTAG.L shares and sells all his long SGP.L holdings") {
    val holdings = Vector(
      Holding(Ticker("SGP.L"), Long,  Volume(100)),
      Holding(Ticker("SGP.L"), Short, Volume(100)),
      Holding(Ticker("CTAG.L"), Long, Volume(100))
    )

    val trader = Trader(traderId, Portfolio(holdings, 100), List(WentLong(date, Ticker("SGP.L"), Bid(10), Volume(100))))
    val event = Sold(date, Ticker("SGP.L"), Ask(200), Volume(100))
    val state = TradingEvents.handleEvent(event)
    val (_, t) = state.run(trader)

    it("should leave only 100 shares Short in SGP.L, and the long CTAG.L shares") {
      t.portfolio.holdings.size shouldEqual 2
      t.portfolio.holdings.head shouldEqual Holding(Ticker("SGP.L"), Short, Volume(100))
      t.portfolio.holdings(1) shouldEqual Holding(Ticker("CTAG.L"), Long, Volume(100))
    }

    it("should add the price achieved by the sale to the traders cash holdings") {
      t.portfolio.cash shouldEqual 20100
    }

    it("should append to the trading event history") {
      t.eventHistory.size shouldEqual 2
    }
  }

  describe("Trader who has 100 shares of SGP.L and recieves a Sold event for 200 shares") {
    val holdings = Vector(
      Holding(Ticker("SGP.L"), Long, Volume(100))
    )

    val trader = Trader(traderId, Portfolio(holdings, 100), List())
    val event = Sold(date, Ticker("SGP.L"), Ask(200), Volume(200))
    val state = TradingEvents.handleEvent(event)
    val (_, t) = state.run(trader)

    it("should entirely remove the holding from their portfolio") {
      t.portfolio.holdings.size shouldEqual 0
    }

    it("should only add cash back for selling 100 shares and not the amount requested") {
      t.portfolio.cash shouldEqual 20100
    }

    it("should append to the trading event history") {
      t.eventHistory.size shouldEqual 1
    }
  }

  describe("Trader who has no holdings at all receives event to sell ") {
    val trader = Trader(traderId, Portfolio(Vector(), 100), List())
    val event = Sold(date, Ticker("SGP.L"), Ask(200), Volume(200))
    val state = TradingEvents.handleEvent(event)
    val (_, t) = state.run(trader)

    it("should not have any effect on the trader except adding an event") {
      t shouldEqual trader.copy(eventHistory = List(event))
    }
  }

  describe("Trader who has 100 shares both Long and Short in his holdings in SGP.L and sells half of his long holdings") {
    val holdings = Vector(
      Holding(Ticker("SGP.L"), Long, Volume(100)),
      Holding(Ticker("SGP.L"), Short, Volume(100))
    )

    val trader = Trader(traderId, Portfolio(holdings, 100), List(WentLong(date, Ticker("SGP.L"), Bid(10), Volume(100))))
    val event = Sold(date, Ticker("SGP.L"), Ask(200), Volume(50))
    val state = TradingEvents.handleEvent(event)
    val (_, t) = state.run(trader)

    it("should still have 2 holdings in his portfolio") {
      t.portfolio.holdings.size shouldBe 2
    }

    it("should append to the trading event history") {
      t.eventHistory.size shouldEqual 2
    }

    it("should leave 100 shares Short in his holdings in SGP.L") {
      t.portfolio.holdings.head shouldEqual Holding(Ticker("SGP.L"), Short, Volume(100))
    }

    it("should add the price achieved by the sale to the traders cash holdings") {
      t.portfolio.cash shouldEqual 10100
    }

    it("should leave 50 shares long in his holdings for SGP.L") {
      val numShares = t.portfolio.holdings.find(e => e.ticker == "SGP.L" && e.direction == Long).map(_.numberOfShares)
      numShares match {
        case Some(e) => e shouldEqual 50
        case None => fail("Trader no longer has a long position in SGP.L")
      }
    }
  }

  describe("Trader who goes long on SGP.L buying 10 shares at 10 pounds with £92 cash in his portfolio") {
    val event = WentLong(date, Ticker("SGP.L"), Bid(poundsAsPence(10)), volume = Volume(10))
    val trader = Trader(traderId, Portfolio(Vector.empty, cash = poundsAsPence(92)), List())
    val state = TradingEvents.handleEvent(event)
    val (_, t) = state.run(trader)

    it("should have 9 shares of SGP.L in his holdings") {
      t.portfolio.holdings.head shouldEqual Holding(Ticker("SGP.L"), Long, Volume(9))
    }

    it("should have only one holding") {
      t.portfolio.holdings.size shouldEqual 1
    }

    it("should have £2 in his cash holdings after the transaction") {
      t.portfolio.cash shouldEqual poundsAsPence(2)
    }

    it("should append to the trading event history") {
      t.eventHistory.size shouldEqual 1
    }
  }

  describe("Trader who goes long on SGP.L buying 100 shares at 16 pounds with £10,000 cash in his portfolio") {
    val event = WentLong(date, Ticker("SGP.L"), Bid(poundsAsPence(16)), volume = Volume(100))

    describe("and doesn't have any holdings yet,") {
      val trader = Trader(traderId, Portfolio(Vector.empty, cash = poundsAsPence(10000)), List(WentLong(date, Ticker("SGP.L"), Bid(10), Volume(100))))
      val state = TradingEvents.handleEvent(event)
      val (_, t) = state.run(trader)

      it("should have 100 shares of SGP.L in his holdings") {
        t.portfolio.holdings.head shouldEqual Holding(Ticker("SGP.L"), Long, Volume(100))
      }

      it("should have only one holding") {
        t.portfolio.holdings.size shouldEqual 1
      }

      it("should have £9200 in his cash holdings after the transaction") {
        t.portfolio.cash shouldEqual poundsAsPence(8400)
      }

      it("should append to the trading event history") {
        t.eventHistory.size shouldEqual 2
      }
    }

    describe("and has a pre-existing SGP Holding of 300 shares along with a short SGP holding and another long holding,") {
      val trader = Trader(traderId, Portfolio(Vector(
        Holding(Ticker("SGP.L"), Long,  Volume(300)),
        Holding(Ticker("SGP.L"), Short, Volume(100)),
        Holding(Ticker("CTAG.L"), Long, Volume(100))
      ), cash = poundsAsPence(10000)), List(WentLong(date, Ticker("SGP.L"), Bid(10), Volume(100))))

      val state = TradingEvents.handleEvent(event)
      val (_, t) = state.run(trader)

      it("should have 400 shares of SGP.L, 100 sold short SGP.L shares and 100 shares of CTAG.L in his holdings") {
        t.portfolio.holdings should contain allOf
          (
            Holding(Ticker("SGP.L"), Long,  Volume(400)),
            Holding(Ticker("SGP.L"), Short, Volume(100)),
            Holding(Ticker("CTAG.L"), Long, Volume(100))
          )
      }

      it("should have 3 holdings") {
        t.portfolio.holdings.size shouldEqual 3
      }

      it("should have £9200 in his cash holdings after the transaction") {
        t.portfolio.cash shouldEqual poundsAsPence(8400)
      }

      it("should append to the trading event history") {
        t.eventHistory.size shouldEqual 2
      }
    }
  }

  describe("When a trader makes 4 trades in succession") {
    val trader = Trader(traderId, Portfolio(Vector(), cash = poundsAsPence(1000)), List.empty)

    val tradingEvents = List(
      WentLong(date, Ticker("CTAG.L"), Bid(10), Volume(100)),
      WentLong(date, Ticker("SGP.L"), Bid(20), Volume(200)),
      Sold(date, Ticker("SGP.L"), Ask(30), Volume(200)),
      Sold(date, Ticker("CTAG.L"), Ask(5), Volume(100))
    )

    val states = TradingEvents.runEvents(trader, tradingEvents)

    it("returns a list of 5 trader states") {
      states.size shouldEqual 5
    }

    it("returns a historical account of every state the trader was in") {
      states(1).portfolio.holdings shouldEqual Vector(Holding(Ticker("CTAG.L"), Long, Volume(100)))
      states(1).portfolio.cash shouldEqual poundsAsPence(990)

      states(2).portfolio.holdings should contain allOf(Holding(Ticker("CTAG.L"), Long, Volume(100)), Holding(Ticker("SGP.L"), Long, Volume(200)))
      states(2).portfolio.cash shouldEqual poundsAsPence(950)

      states(3).portfolio.holdings shouldEqual Vector(Holding(Ticker("CTAG.L"), Long, Volume(100)))
      states(3).portfolio.cash shouldEqual poundsAsPence(1010)

      states(4).portfolio.holdings shouldEqual Vector()
      states(4).portfolio.cash shouldEqual poundsAsPence(1015)
    }

    it("returns the start state of the trader at the start of the list of states") {
      states.head.portfolio.holdings shouldEqual Vector()
      states.head.portfolio.cash shouldEqual poundsAsPence(1000)
    }

    it("Trading history should increase by 1 event after every state change") {
      for (i <- 1 until 4) {
        states(i).eventHistory.size shouldEqual i
      }
    }
  }

  describe("Trader who has a dividend paid of £10.51") {
    val holdings = Vector(
      Holding(Ticker("SGP.L"), Long, Volume(100)),
      Holding(Ticker("SGP.L"), Short, Volume(100))
    )

    val trader = Trader(traderId, Portfolio(holdings, 5000), List(WentLong(date, Ticker("SGP.L"), Bid(10), Volume(100))))
    val event = DividendPaid(date, Ticker("SGP.L"), 1051)
    val state = TradingEvents.handleEvent(event)
    val (_, t) = state.run(trader)

    it("should have have the dividend cash added to his current cash position") {
      t.portfolio.cash shouldEqual 6051
    }

    it("should have the dividend paid event stored in his history") {
      t.eventHistory.size shouldEqual 2
      t.eventHistory(1) shouldEqual event
    }

    it("should not have their amount of holdings amended") {
      t.portfolio.holdings.size shouldEqual 2
    }
  }
}
