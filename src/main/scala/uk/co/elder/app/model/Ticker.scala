package uk.co.elder.app.model

import scalaz.ValidationNel
import scalaz.syntax.validation._

case class Ticker(value: String) extends AnyVal

object Ticker {
  def validateTicker(t: Ticker): ValidationNel[String, Ticker] = {
    if (t.value.isEmpty) "The ticker value is empty".failureNel else t.successNel
  }
}