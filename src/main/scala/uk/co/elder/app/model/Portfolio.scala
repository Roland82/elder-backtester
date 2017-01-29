package uk.co.elder.app.model

import scalaz.Reader

case class Portfolio(holdings: Vector[Holding], cash: BigDecimal)

object Portfolio {

}
