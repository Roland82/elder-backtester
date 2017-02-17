package uk.co.elder.app.model

import scalaz.{ValidationNel}
import scalaz.syntax.validation._
import shapeless.tag.@@
import shapeless.tag

sealed trait Ticker

object Ticker {
  def apply(t: String): String @@ Ticker = tag.apply[Ticker](t)

  def validateTicker(t: String @@ Ticker): ValidationNel[String, String @@ Ticker] = {
    if (t.isEmpty) "The ticker value is empty".failureNel else t.successNel
  }
}