package sentiment

import org.scalatest._
import flatspec._
import matchers._

import sentiment.utils.SentimentUtils

class SentimentUtilsTest extends AnyFlatSpec with should.Matchers {

  "SentimentUtils" should "calculate allCapDifferential" in {
    SentimentUtils.allCapDifferential(Seq("VADER", "is", "handsome", "and", "funny")) shouldEqual true
    SentimentUtils.allCapDifferential(Seq("VADER", "is", "HANDSOME", "and", "funny")) shouldEqual true
    SentimentUtils.allCapDifferential(Seq("VADER", "is", "HANDSOME", "AND", "FUNNY")) shouldEqual true

    SentimentUtils.allCapDifferential(Seq("vader", "is", "handsome", "and", "funny")) shouldEqual false
    SentimentUtils.allCapDifferential(Seq("VADER", "IS", "HANDSOME", "AND", "FUNNY")) shouldEqual false
    SentimentUtils.allCapDifferential(Seq("the", "book", "was", "kinda", "good")) shouldEqual false
    SentimentUtils.allCapDifferential(Seq("THE", "BOOK", "WAS", "KINDA", "GOOD")) shouldEqual false

    SentimentUtils.allCapDifferential(Seq("GerVADER", "hat", "viel", "Potential")) shouldEqual false
    SentimentUtils.allCapDifferential(Seq("GERVADER", "HAT", "VIEL", "POTENTIAL")) shouldEqual false
  }

  "SentimentUtils" should "normalize" in {
    SentimentUtils.normalize(4) shouldEqual 0.7184212081070996
    SentimentUtils.normalize(3) shouldEqual 0.6123724356957946
    SentimentUtils.normalize(1, 30) shouldEqual 0.1796053020267749
    SentimentUtils.normalize(0) shouldEqual 0.0
    SentimentUtils.normalize(-1) shouldEqual -0.25
  }

  "SentimentUtils" should "isUpper" in {
    SentimentUtils.isUpper("VADER") shouldEqual true
    SentimentUtils.isUpper("vader") shouldEqual false
    SentimentUtils.isUpper("Vader") shouldEqual false
  }

  "SentimentText" should "negated" in {
  }

  "SentimentText" should "scalarIncDec" in {
  }
}
