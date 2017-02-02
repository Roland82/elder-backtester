package uk.co.elder

import scalaz.{@@, Tag}

/**
  * Created by rolandormrod on 02/02/2017.
  */
package object app {
  implicit class BigDecimalExt(v: BigDecimal) {
    implicit def *[T](tagged: BigDecimal @@ T): BigDecimal = {
      v * Tag.unwrap(tagged)
    }
  }
}
