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

  "SentimentUtils" should "negated" in {
    SentimentUtils.negated(List("It", "isn't", "a", "horrible", "book"), true) shouldEqual true
    SentimentUtils.negated(List("I", "am", "not", "mad")) shouldEqual true
    SentimentUtils.negated(List("You", "are", "mad")) shouldEqual false
    // should fail cause it isn't a negation
    SentimentUtils.negated(List("managment")) shouldEqual false
  }

  "SentimentUtils" should "scalarIncDec" in {
    // dampener
    SentimentUtils.scalarIncDec("kinda", 1, false) shouldEqual -0.293
    SentimentUtils.scalarIncDec("little", -1, false) shouldEqual 0.293

    // booster
    SentimentUtils.scalarIncDec("absolutely", 1, true) shouldEqual 0.293
    SentimentUtils.scalarIncDec("completely", -1, true) shouldEqual -0.293

    // booster is ALLCAPS & no cap differential
    SentimentUtils.scalarIncDec("FABULOUSLY", 1, false) shouldEqual 0.293
    SentimentUtils.scalarIncDec("FABULOUSLY", -1, false) shouldEqual -0.293

    // dampener word is ALLCAPS & cap differential
    // 0.733 = (empirically derived mean sentiment intensity
    // rating increase for using ALLCAPs to emphasize a word)
    SentimentUtils.scalarIncDec("BARELY", 1, true) shouldEqual -0.293+0.733
    SentimentUtils.scalarIncDec("BARELY", -1, true) shouldEqual 0.293-0.733

    // word is not in booster dic
    SentimentUtils.scalarIncDec("lol", -1, true) shouldEqual 0.0
  }
}
