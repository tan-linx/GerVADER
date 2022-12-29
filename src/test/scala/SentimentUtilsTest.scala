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

  "SentimentUtils" should "getPolarity" in {
    SentimentUtils.getPolarity(-0.34) shouldEqual "negative"
    SentimentUtils.getPolarity(0.0) shouldEqual "neutral"
    SentimentUtils.getPolarity(0.34) shouldEqual "positive"
  }

  "SentimentUtils" should "negated" in {
    //SentimentUtils.negated(List("It", "isn't", "a", "horrible", "book"), true) shouldEqual true
    SentimentUtils.negated(List("Es", "ist", "kein", "schlechtes", "Buch"), true) shouldEqual true
    SentimentUtils.negated(List("Ich", "kenne", "keinen", "Ausweg"), true) shouldEqual true
    SentimentUtils.negated(List("Ich", "hab", "keinen", "Plan"), true) shouldEqual true
    SentimentUtils.negated(List("Sie", "schreckten", "vor", "keinem", "Verbrechen", "zurück"), true) shouldEqual true
    SentimentUtils.negated(List("Ich", "bin", "nicht", "sauer")) shouldEqual true
    SentimentUtils.negated(List("das", "würde", "ich", "niemals", "tun")) shouldEqual true
    // should fail cause there is no negation
    SentimentUtils.negated(List("Du", "bist", "sauer")) shouldEqual false
    SentimentUtils.negated(List("managment")) shouldEqual false
    // enthält n't
    SentimentUtils.negated(List("I", "can't", "believe", "you", "support", "sanctions")) shouldEqual true
  }

  "SentimentUtils" should "scalarIncDec" in {
    // dampener & no cap differential
    SentimentUtils.scalarIncDec("irgendwie", 1, false) shouldEqual -0.293
    SentimentUtils.scalarIncDec("bisschen", -1, false) shouldEqual 0.293

    // booster & cap differential
    SentimentUtils.scalarIncDec("absolut", 1, true) shouldEqual 0.293
    SentimentUtils.scalarIncDec("total", -1, true) shouldEqual -0.293

    // booster is ALLCAPS & no cap differential
    SentimentUtils.scalarIncDec("SAGENHAFT", 1, false) shouldEqual 0.293
    SentimentUtils.scalarIncDec("SAGENHAFT", -1, false) shouldEqual -0.293

    // dampener word is ALLCAPS & cap differential
    // 0.733 = (empirically derived mean sentiment intensity
    // rating increase for using ALLCAPs to emphasize a word)
    SentimentUtils.scalarIncDec("KAUM", 1, true) shouldEqual -0.293+0.733
    SentimentUtils.scalarIncDec("KAUM", -1, true) shouldEqual 0.293-0.733

    // word is not in booster dic
    SentimentUtils.scalarIncDec("lol", -1, true) shouldEqual 0.0
  }
}
