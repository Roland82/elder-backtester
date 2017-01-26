import org.scalatest._
import uk.co.elder.app.model._
import uk.co.elder.app.{Long, Short, Sold, SoldShort, Ticker, TradingEvents, WentLong}

class TraderSpec extends FunSpec with Matchers {
  def poundsAsPence(pounds: Int) = pounds * 100

  describe("Trader who has 100 shares both Long and Short in his holdings in SGP.L") {
    val holdings = Vector(
      Holding(Ticker("SGP.L"), Long, 100),
      Holding(Ticker("SGP.L"), Short, 100)
    )

    val trader = Trader(Portfolio(holdings, 100))
    val event = Sold(Ticker("SGP.L"), Ask(200))
    val state = TradingEvents.handleEvent(event)
    val (_, t) = state.run(trader)

    it("should sell all 100 long positions in SGP.L") {
      t.portfolio.holdings.size shouldEqual 1
    }

    it("should leave 100 shares Short in his holdings in SGP.L") {
      t.portfolio.holdings.head shouldEqual Holding(Ticker("SGP.L"), Short, 100)
    }

    it("should add the price achieved by the sale to the traders cash holdings") {
      t.portfolio.cash shouldEqual 20100
    }
  }

  describe("Trader who goes long on SGP.L buying 100 shares at 16 pounds with £10,000 cash in his portfolio") {
    val event = WentLong(Ticker("SGP.L"), Bid(poundsAsPence(16)), numberOfShares = 100)

    describe("and doesn't have any holdings yet,") {
      val trader = Trader(Portfolio(Vector.empty, cash = poundsAsPence(10000)))
      val state = TradingEvents.handleEvent(event)
      val (_, t) = state.run(trader)

      it("should have 100 shares of SGP.L in his holdings") {
        t.portfolio.holdings.head shouldEqual Holding(Ticker("SGP.L"), Long, 100)
      }

      it("should have only one holding") {
        t.portfolio.holdings.size shouldEqual 1
      }

      it("should have £9200 in his cash holdings after the transaction") {
        t.portfolio.cash shouldEqual poundsAsPence(8400)
      }
    }

    describe("and has a pre-existing SGP Holding of 300 shares along with a short SGP holding and another long holding,") {
      val trader = Trader(Portfolio(Vector(
        Holding(Ticker("SGP.L"), Long, 300),
        Holding(Ticker("SGP.L"), Short, 100),
        Holding(Ticker("CTAG.L"), Long, 100)
      ), cash = poundsAsPence(10000)))

      val state = TradingEvents.handleEvent(event)
      val (_, t) = state.run(trader)

      it("should have 400 shares of SGP.L, 100 sold short SGP.L shares and 100 shares of CTAG.L in his holdings") {
        t.portfolio.holdings should contain allOf
          (
            Holding(Ticker("SGP.L"), Long, 400),
            Holding(Ticker("SGP.L"), Short, 100),
            Holding(Ticker("CTAG.L"), Long, 100)
          )
      }

      it("should have 3 holdings") {
        t.portfolio.holdings.size shouldEqual 3
      }

      it("should have £9200 in his cash holdings after the transaction") {
        t.portfolio.cash shouldEqual poundsAsPence(8400)
      }
    }
  }

  describe("When a trader makes 4 trades in succession") {
    val trader = Trader(Portfolio(Vector(), cash = poundsAsPence(1000)))

    val tradingEvents = List(
      WentLong(Ticker("CTAG.L"), Bid(10), 100),
      WentLong(Ticker("SGP.L"), Bid(20), 200),
      Sold(Ticker("SGP.L"), Ask(30)),
      Sold(Ticker("CTAG.L"), Ask(5))
    )

    val states = TradingEvents.runEvents(trader, tradingEvents)

    it("returns a list of 5 trader states") {
      states.size shouldEqual 5
    }

    it("returns a historical account of every state the trader was in") {
      states(1).portfolio.holdings shouldEqual Vector(Holding(Ticker("CTAG.L"), Long, 100))
      states(1).portfolio.cash shouldEqual poundsAsPence(990)

      states(2).portfolio.holdings should contain allOf (Holding(Ticker("CTAG.L"), Long, 100), Holding(Ticker("SGP.L"), Long, 200))
      states(2).portfolio.cash shouldEqual poundsAsPence(950)

      states(3).portfolio.holdings shouldEqual Vector(Holding(Ticker("CTAG.L"), Long, 100))
      states(3).portfolio.cash shouldEqual poundsAsPence(1010)

      states(4).portfolio.holdings shouldEqual Vector()
      states(4).portfolio.cash shouldEqual poundsAsPence(1015)
    }

    it("returns the start state of the trader at the start of the list of states") {
      states.head.portfolio.holdings shouldEqual Vector()
      states.head.portfolio.cash shouldEqual poundsAsPence(1000)
    }
  }
}
