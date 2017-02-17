package uk.co.elder.app.model

import shapeless.tag.{@@}
import shapeless.tag
import scalaz.ValidationNel
import scalaz.syntax.validation._

case class PriceData(ask: BigDecimal @@ Ask, mid: BigDecimal @@ Mid, bid: BigDecimal @@ Bid)

sealed trait Ask
object Ask {
  def apply(v: BigDecimal): BigDecimal @@ Ask = tag.apply[Ask](v)

  def validateAsk(b: BigDecimal @@ Ask): ValidationNel[String, BigDecimal @@ Ask] = {
    if (b <= 0) "The Ask price must be greater than 0".failureNel else b.successNel
  }
}
sealed trait Mid

object Mid {
  def apply(v: BigDecimal): BigDecimal @@ Mid = tag.apply[Mid](v)
}

sealed trait Bid

object Bid {
  def apply(v: BigDecimal): BigDecimal @@ Bid = tag.apply[Bid](v)

  def validateBid(b: BigDecimal @@ Bid): ValidationNel[String, BigDecimal @@ Bid] = {
    if (b <= 0) "The Bid price must be greater than 0".failureNel else b.successNel
  }
}
