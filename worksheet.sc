import uk.co.elder.app.{Holding, Long, Portfolio, Ticker, TradingEvents}
import uk.co.elder.app.model.{Ask, Portfolio, Trader}

import scalaz.{Lens, Store}

val portfolio = Portfolio(Vector.empty, 100)
val trader = TradingEvents(portfolio)

val changePortfolio = Lens.lensu[Trader, Portfolio]((t, p) => t.copy(p), e => e.portfolio)

val changeCash =
  Lens.lensu[Portfolio, BigDecimal]((p, c) => p.copy(cash = c), e => e.cash)

(changePortfolio andThen changeCash).mods(i => 1000).run(trader)

