package uk.co.elder.app.model

import scalaz.{@@, Tag, ValidationNel}
import scalaz.syntax.validation._

sealed trait Ticker

object Ticker {
  def apply(t: String): String @@ Ticker = Tag[String, Ticker](t)

  def validateTicker(t: String @@ Ticker): ValidationNel[String, String @@ Ticker] = {
    if (Tag.unwrap(t).isEmpty) "The ticker value is empty".failureNel else t.successNel
  }
}