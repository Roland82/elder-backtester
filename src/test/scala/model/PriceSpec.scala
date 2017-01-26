package model

import org.scalatest.{FunSpec, Matchers}
import uk.co.elder.app.model.{Ask, Bid, Mid, PriceData}

/**
  * Created by rolandormrod on 23/01/2017.
  */
class PriceSpec extends FunSpec with Matchers {
  describe("PriceData") {
    it("should calculate the spread price correctly") {
      val priceData = PriceData(Ask(100), Mid(101), Bid(102))
      priceData.spread() shouldEqual 2
    }

    it("should calculate the spread percentage correctly") {
      val priceData = PriceData(Ask(200), Mid(202), Bid(205))
      priceData.spreadAsPercentage() shouldEqual 2.5
    }
  }
}
