package uk.co.elder.app.model

import scalaz.{@@, Tag, ValidationNel}
import scalaz.syntax.validation._

case class PriceData(ask: BigDecimal @@ Ask, mid: BigDecimal @@ Mid, bid: BigDecimal @@ Bid)

sealed trait Ask
object Ask {
  def apply(v: BigDecimal): BigDecimal @@ Ask = Tag[BigDecimal, Ask](v)

  def validateAsk(b: BigDecimal @@ Ask): ValidationNel[String, BigDecimal @@ Ask] = {
    if (Tag.unwrap(b) <= 0) "The Ask price must be greater than 0".failureNel else b.successNel
  }
}
sealed trait Mid

object Mid {
  def apply(v: BigDecimal): BigDecimal @@ Mid = Tag[BigDecimal, Mid](v)
}

sealed trait Bid

object Bid {
  def apply(v: BigDecimal): BigDecimal @@ Bid = Tag[BigDecimal, Bid](v)

  def validateBid(b: BigDecimal @@ Bid): ValidationNel[String, BigDecimal @@ Bid] = {
    if (Tag.unwrap(b) <= 0) "The Bid price must be greater than 0".failureNel else b.successNel
  }
}
