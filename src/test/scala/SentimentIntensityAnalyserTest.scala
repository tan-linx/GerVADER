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

    val complexTest = analyzer.polarityScores("The plot was good, but the characters are uncompelling and the dialog is not great.")
    complexTest.negative shouldEqual 0.327
    complexTest.neutral shouldEqual 0.579
    complexTest.positive shouldEqual 0.094
    complexTest.compound shouldEqual -0.7042

    val negationTest = analyzer.polarityScores("it isn't an horrible book.")
    negationTest.compound shouldEqual 0.431
    negationTest.negative shouldEqual 0.0
    negationTest.neutral shouldEqual 0.584
    negationTest.positive shouldEqual 0.416

    val exampleWithA = analyzer.polarityScores("VADER is such a badass!")
    exampleWithA.compound shouldEqual 0.4003
    exampleWithA.negative shouldEqual 0.0
    exampleWithA.neutral shouldEqual 0.598
    exampleWithA.positive shouldEqual 0.402

    val kindOfTest = analyzer.polarityScores("The book was kind of good.")
    kindOfTest.negative shouldEqual 0
    kindOfTest.neutral shouldEqual 0.657
    kindOfTest.positive shouldEqual 0.343
    kindOfTest.compound shouldEqual 0.3832

    val onlyKindOfTest = analyzer.polarityScores("The book was only kind of good.")
    onlyKindOfTest.compound shouldEqual 0.3832
    onlyKindOfTest.negative shouldEqual 0.0
    onlyKindOfTest.neutral shouldEqual 0.697
    onlyKindOfTest.positive shouldEqual 0.303

    val emojiTest = analyzer.polarityScores("Catch utf-8 emoji such as 💘 and 💋 and 😁")
    emojiTest.compound shouldEqual 0.875
    emojiTest.negative shouldEqual 0.0
    emojiTest.neutral shouldEqual  0.583
    emojiTest.positive shouldEqual 0.417

    val noNegationTest = analyzer.polarityScores("I prefer no sanctions")
    noNegationTest.compound shouldEqual -0.296

    val emojiTest2 = analyzer.polarityScores("what a stupid 🤡🤡🤡")
    emojiTest2.compound shouldEqual -0.5267
    analyzer.polarityScores("Without a doubt, an excellent idea").compound shouldEqual 0.7013
    analyzer.polarityScores("With VADER, sentiment analysis is the shit!").compound shouldEqual 0.6476
    analyzer.polarityScores("On the other hand, VADER is quite bad ass").compound shouldEqual 0.802
    analyzer.polarityScores("Sentiment analysis has never been this good!").compound shouldEqual 0.5672
    analyzer.polarityScores("Most automated sentiment analysis tools are shit.").compound shouldEqual -0.5574
    analyzer.polarityScores("Other sentiment analysis tools can be quite bad.").compound shouldEqual -0.5849
    analyzer.polarityScores("Roger Dodger is one of the most compelling variations on this theme.").compound shouldEqual 0.2944
    analyzer.polarityScores("Roger Dodger is at least compelling as a variation on the theme.").compound shouldEqual 0.2263
    analyzer.polarityScores(" Not such a badass after all.").compound shouldEqual -0.2584
    analyzer.polarityScores("VADER is very smart, handsome, and funny").compound shouldEqual 0.8545
    analyzer.polarityScores("VADER is VERY SMART, handsome, and FUNNY.").compound shouldEqual 0.9227
    analyzer.polarityScores("VADER is VERY SMART, uber handsome, and FRIGGIN FUNNY!!!").compound shouldEqual 0.9469
    analyzer.polarityScores("Today SUX!").compound shouldEqual -0.5461
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

  "A SentimentIntensityAnalyzer" should "scoreValence" in {
    val analyzer = new SentimentIntensityAnalyzer()
    // no sentimens
    analyzer.scoreValence(Seq(), "Idk. ") shouldEqual SentimentAnalysisResults(
      compound = 0.0,
      positive = 0.0,
      negative = 0.0,
      neutral = 0.0
    )

    // no exclamation
    analyzer.scoreValence(Seq(1.85), "it isn't a horrible book.") shouldEqual SentimentAnalysisResults(
      compound = 0.431,
      positive = 1.0,
      negative = 0.0,
      neutral = 0.0
    )

    // punctuation emphasis
    // question emphasis
    // 0 or 1
    analyzer.scoreValence(Seq(1.7, 1.1), "Do you support the sanctions as well?") shouldEqual SentimentAnalysisResults(
      compound = 0.5859,
      positive = 1.0,
      negative = 0.0,
      neutral = 0.0
    )
    // 2-3
     analyzer.scoreValence(Seq(1.7, 1.1), "Do you support the sanctions as well??") shouldEqual SentimentAnalysisResults(
      compound = 0.6322,
      positive = 1.0,
      negative = 0.0,
      neutral = 0.0
    )

    // 3+
    analyzer.scoreValence(Seq(1.7, 1.1), "Do you support the sanctions as well????") shouldEqual SentimentAnalysisResults(
      compound = 0.6966,
      positive = 1.0,
      negative = 0.0,
      neutral = 0.0
    )

    // exclamation emphasis
    analyzer.scoreValence(Seq(-1.9), "Sanctions suck!!!") shouldEqual SentimentAnalysisResults(
      compound = -0.5826,
      positive = 0.0,
      negative = 1.0,
      neutral = 0.0
    )

    // more than 4 exclamation points
    val fourExclValence = analyzer.scoreValence(Seq(-1.9), "Sanctions suck!!!!")
    analyzer.scoreValence(Seq(-1.9), "Sanctions suck!!!!!!") shouldEqual fourExclValence
  }

  "A SentimentIntensityAnalyzer" should "sentimentValence" in {
    val analyzer = new SentimentIntensityAnalyzer()
    // lexikon does not contain item
    analyzer.sentimentValence(1.0, new SentiText("It isn't a horrible book."), "book", 4) shouldEqual 1.0
    // lexikon contains item never check
    analyzer.sentimentValence(1.0, new SentiText("It isn't a horrible book."), "horrible", 3) shouldEqual 1.85 // nevercheck -0.74*-2.0
    // weird results not equal to original vader
    analyzer.sentimentValence(0.0, new SentiText("It isn't a incredibly horrible book."), "horrible", 4) shouldEqual 2.06682
  }

  "A SentimentIntensityAnalyzer" should "makeLexDict" in {
    val analyzer = new SentimentIntensityAnalyzer()
    val lexikon = analyzer.makeLexDict()
    lexikon("$:") shouldEqual -1.5
  }

  "A SentimentIntensityAnalyzer" should "makeEmojiDict" in {
    val analyzer = new SentimentIntensityAnalyzer()
    val emoji_lexikon = analyzer.makeEmojiDict()
    emoji_lexikon("💔") shouldEqual "broken heart"
    emoji_lexikon("🤡") shouldEqual "clown face"
    emoji_lexikon("🤣") shouldEqual "rolling on the floor laughing"
    emoji_lexikon("😂") shouldEqual "face with tears of joy"
  }

  /**
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
