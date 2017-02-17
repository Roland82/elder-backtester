package uk.co.elder.app.model

import shapeless.tag
import shapeless.tag.@@

import scalaz.{ValidationNel}
import scalaz.syntax.validation._

sealed trait Volume

object Volume {
  def apply(t: BigInt): BigInt @@ Volume = tag.apply[Volume](t)
  def validateVolume(volume: BigInt @@ Volume): ValidationNel[String, BigInt @@ Volume] = {
    if (volume <= 0) "Volume cannot be less than or equal to 0 shares".failureNel else volume.successNel
  }
}

