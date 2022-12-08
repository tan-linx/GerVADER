import org.scalatest._
import flatspec._
import matchers._

import sentiment.SentimentIntensityAnalyzer

class SentimentIntensityAnalyserTest extends AnyFlatSpec with should.Matchers {
  "A SentimentIntensityAnalyzer" should "calculate polarity scores (English)" in {
    val analyzer = new SentimentIntensityAnalyzer

    val standardGoodTest = analyzer.polarityScores("VADER is smart, handsome, and funny.")
    standardGoodTest.negative shouldEqual 0
    standardGoodTest.neutral shouldEqual 0.254
    standardGoodTest.positive shouldEqual 0.746
    standardGoodTest.compound shouldEqual 0.8316

    val kindOfTest = analyzer.polarityScores("The book was kind of good.")
    //Failing
    kindOfTest.negative shouldEqual 0
    kindOfTest.neutral shouldEqual 0.657
    kindOfTest.positive shouldEqual 0.343
    kindOfTest.compound shouldEqual 0.3832

    val complexTest = analyzer.polarityScores("The plot was good, but the characters are uncompelling and the dialog is not great.")
    complexTest.negative shouldEqual 0.327
    complexTest.neutral shouldEqual 0.579
    complexTest.positive shouldEqual 0.094
    complexTest.compound shouldEqual -0.7042
  }

  "A SentimentIntensityAnalyzer" should "calculate polarity scores (German)" in {
    val analyzer = new SentimentIntensityAnalyzer

    val testGood = analyzer.polarityScores("GerVADER hat viel Potential <3")
    testGood.negative shouldEqual 0.0
    testGood.neutral shouldEqual 0.58
    testGood.positive shouldEqual 0.42
    testGood.compound shouldEqual 0.4404

    val testGood2 = analyzer.polarityScores("Fußball entfacht in mir ein Feuer der Leidenschaft.")
    testGood2.negative shouldEqual 0.174
    testGood2.neutral shouldEqual 0.496
    testGood2.positive shouldEqual 0.331
    testGood2.compound shouldEqual 0.4404

    val testNegative = analyzer.polarityScores("gwroße zeitverschwendung heute >:/")
    testNegative.negative shouldEqual 0.726
    testNegative.neutral shouldEqual 0.274
    testNegative.positive shouldEqual 0.0
    testNegative.compound shouldEqual -0.6486

    val testNegative2 = analyzer.polarityScores("ich finde das überhaupt nicht gut")
    testNegative2.negative shouldEqual 0.338
    testNegative2.neutral shouldEqual 0.662
    testNegative2.positive shouldEqual 0.0
    testNegative2.compound shouldEqual -0.3724
  }
}
