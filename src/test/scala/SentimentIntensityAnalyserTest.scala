package sentiment

import org.scalatest._
import flatspec._
import matchers._
import scala.collection.mutable.ListBuffer

import sentiment.utils.SentimentUtils

class SentimentIntensityAnalyserTest extends AnyFlatSpec with should.Matchers {
  "A SentimentIntensityAnalyzer" should "calculate polarity scores (English)" in {
    val analyzer = new SentimentIntensityAnalyzer

    //success
    val standardGoodTest = analyzer.polarityScores("VADER is smart, handsome, and funny.")
    standardGoodTest.negative shouldEqual 0
    standardGoodTest.neutral shouldEqual 0.254
    standardGoodTest.positive shouldEqual 0.746
    standardGoodTest.compound shouldEqual 0.8316

    val kindOfTest = analyzer.polarityScores("The book was kind of good.")
    print(kindOfTest)
    kindOfTest.negative shouldEqual 0
    //kindOfTest.neutral shouldEqual 0.657
    //kindOfTest.positive shouldEqual 0.343
    //kindOfTest.compound shouldEqual 0.3832

    val complexTest = analyzer.polarityScores("The plot was good, but the characters are uncompelling and the dialog is not great.")
    complexTest.negative shouldEqual 0.327
    complexTest.neutral shouldEqual 0.579
    complexTest.positive shouldEqual 0.094
    complexTest.compound shouldEqual -0.7042

    //it isn't a horrible book.---------------------------------------- {'neg': 0.0, 'neu': 0.584, 'pos': 0.416, 'compound': 0.431}
    val negationTest = analyzer.polarityScores("it isn't a horrible book.")
    negationTest.negative shouldEqual 0.0
    negationTest.neutral shouldEqual 0.584
    negationTest.positive shouldEqual 0.416
    negationTest.compound shouldEqual 0.431
  }

  "A SentimentIntensityAnalyzer" should "neverCheck" in {
    val analyzer = new SentimentIntensityAnalyzer
    // 1word preceding lexicon word (w/o stopwords)
    analyzer.neverCheck(1.0, Seq("It", "isn't", "a", "horrible", "book", ":)"), 0, 2) shouldEqual -0.74
    // 2 word preceding lexicon word (w/o stopwords)
    analyzer.neverCheck(1.0, Seq("she", "wont", "do", "this",":)"), 1, 3) shouldEqual -0.74
    // 3 word preceding lexicon word (w/o stopwords)
    analyzer.neverCheck(1.0, Seq("I", "cant", "do", "my", "homework"), 2, 4) shouldEqual -0.74
    // 3 word preceding the lexikon, word preceding the lexikon is "this", "so"
    analyzer.neverCheck(1.0, Seq("I", "cant", "do", "this",":-)"), 2, 4) shouldEqual 1.25
    analyzer.neverCheck(1.0, Seq("she", "wont", "do", "so",":-)"), 2, 4) shouldEqual 1.25
    // word preceding the lexikon is "never this/so"
    analyzer.neverCheck(1.0, Seq("I", "was", "never", "so", "thankful", "to", "see", "a", "friendly", "face"), 1, 4) shouldEqual 1.25
    analyzer.neverCheck(1.0, Seq("I", "was", "never", "this", "thankful", "to", "see", "a", "friendly", "face"), 1, 4) shouldEqual 1.25
    analyzer.neverCheck(1.0, Seq("I", "was", "never", "so", "thankful", "to", "see", "a", "friendly", "face"), 2, 4) shouldEqual 1.25
    analyzer.neverCheck(1.0, Seq("I", "was", "never", "this", "thankful", "to", "see", "a", "friendly", "face"), 2, 4) shouldEqual 1.25
    analyzer.neverCheck(1.0, Seq("I", "like", "you"), 0, 1) shouldEqual 1.0
  }


  "A SentimentIntensityAnalyzer" should "sentimentValence" in {
    //(valenc: Double, sentiText: SentiText, item: String, i: Int, sentiments: ListBuffer[Double])
    val analyzer = new SentimentIntensityAnalyzer()
    // lexikon does not contain item
    analyzer.sentimentValence(1.0, new SentiText("It isn't a horrible book."), "book", 4, ListBuffer(1.85)) shouldEqual (1.0, ListBuffer(1.85, 1.0)) //normalized 0.67082
    // lexikon contains item never check
    analyzer.sentimentValence(1.0, new SentiText("It isn't a horrible book."), "horrible", 3, ListBuffer()) shouldEqual (1.85, ListBuffer(1.85)) // nevercheck -0.74*-2.0
    // weird results not equal to original vader
    //analyzer.sentimentValence(0.0, new SentiText("It isn't a incredibly horrible book."), "horrible", 4, ListBuffer(0.0, 0, 0, 2.06682)) shouldEqual (2.06682, ListBuffer(0.0, 0, 0, 2.06682))
  }

  "A SentimentIntensityAnalyzer" should "butCheck" in {
    val analyzer = new SentimentIntensityAnalyzer()

    analyzer.butCheck(Seq("not", "bad", "but", "not", "good"), ListBuffer(1, 1, 0, 1, 1)) shouldEqual ListBuffer(0.5, 0.5, 0, 1.5, 1.5)
    analyzer.butCheck(Seq("not", "bad", "BUT", "without", "a", "doubt", "uncreative"), ListBuffer(1, 1, 0, 1, 1, 1, 1)) shouldEqual ListBuffer(0.5, 0.5, 0, 1.5, 1.5, 1.5, 1.5)
    analyzer.butCheck(Seq("not", "bad", "buT", "without", "a", "doubt", "uncreative"), ListBuffer(1, 1, 0, 1, 1, 1, 1)) shouldEqual ListBuffer(0.5, 0.5, 0, 1.5, 1.5, 1.5, 1.5)

    // no but
    analyzer.butCheck(Seq("I", "cant", "do", "this",":-)"), ListBuffer(1, 1, 1, 1, 1)) shouldEqual ListBuffer(1, 1, 1, 1, 1)
  }

  "A SentimentIntensityAnalyzer" should "leastcheck" in {
    val analyzer = new SentimentIntensityAnalyzer()

    val valence = 1
    analyzer.leastCheck(valence, Seq("you", "are", "the", "least", "stupid", "person", "i", "know"), 4) shouldEqual -0.74
    analyzer.leastCheck(valence, Seq("least", "smart", "animal"), 1) shouldEqual -0.74

    // at least/very least preceding word at given index doesn't modify valence
    analyzer.leastCheck(valence, Seq("At", "least", "he", "likes", "you"), 2) shouldEqual 1.0
    analyzer.leastCheck(valence, Seq("At", "the", "very", "least", "you", "should"), 4) shouldEqual 1.0
  }

  "A SentimentIntensityAnalyzer" should "idiomsCheck" in {
    val analyzer = new SentimentIntensityAnalyzer()
    analyzer.idiomsCheck(0.0, Seq("VADEr", "is", "the", "shit"), 3) shouldEqual 3
    analyzer.idiomsCheck(0.0, Seq("VADER", "is", "the", "bomb"), 3) shouldEqual 3
   //analyzer.idiomsCheck(0.0, Seq("VADER", "is", "the", "bomb"), 0) shouldEqual 3

    // booster/dampener check
    analyzer.idiomsCheck(0.0, Seq("The", "book", "was", "kind", "of", "good"), 5) shouldEqual -0.293
    analyzer.idiomsCheck(0.0, Seq("The", "book", "was", "sort", "of", "good"), 5) shouldEqual -0.293
    // word at index 4 of
    analyzer.idiomsCheck(0.0, Seq("The", "book", "was", "sort", "of", "good"), 4) shouldEqual 0
  }

  /**
   * leastcheck and special case idioms
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
  } */
}
