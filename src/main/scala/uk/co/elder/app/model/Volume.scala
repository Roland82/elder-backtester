package uk.co.elder.app.model

import scalaz.ValidationNel
import scalaz.syntax.validation._
case class Volume(value: BigInt) extends AnyVal

object Volume {
  def validateVolume(volume: Volume): ValidationNel[String, Volume] = {
    if (volume.value <= 0) "Volume cannot be less than or equal to 0 shares".failureNel else volume.successNel
  }
}

