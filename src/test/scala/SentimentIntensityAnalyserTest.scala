package sentiment

import org.scalatest._
import flatspec._
import matchers._
import scala.collection.mutable.ListBuffer

import sentiment.utils.SentimentUtils

class SentimentIntensityAnalyserTest extends AnyFlatSpec with should.Matchers {
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

    val testGood3 = analyzer.polarityScores("Kaltes Eis, kalter Pool und die Sonne die mich anlacht, schön schön")
    testGood3.negative shouldEqual 0.223
    testGood3.neutral shouldEqual 0.426
    testGood3.positive shouldEqual 0.351
    testGood3.compound shouldEqual 0.5267

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

    val testNegative3 = analyzer.polarityScores("richtiger scheiß alter")
    testNegative3.negative shouldEqual 0.634
    testNegative3.neutral shouldEqual 0.0
    testNegative3.positive shouldEqual 0.366
    testNegative3.compound shouldEqual -0.2263

    val testNeutral = analyzer.polarityScores("Danach bin ich nach Hause gegangen")
    testNeutral.negative shouldEqual 0.0
    testNeutral.neutral shouldEqual 1.0
    testNeutral.positive shouldEqual 0.0
    testNeutral.compound shouldEqual 0.0

    val testNeutral2 = analyzer.polarityScores("Katzen miauen manchmal wenn der Halter fortgeht.")
    testNeutral2.negative shouldEqual 0.0
    testNeutral2.neutral shouldEqual 1.0
    testNeutral2.positive shouldEqual 0.0
    testNeutral2.compound shouldEqual 0.0

    // test added step in GerVADER: in the lexicon, words can have a different intensity whether it is for example a noun (written in capital in German) or a verb.  e.g. Anstieg and anstieg
    analyzer.polarityScores("der Anstieg der Kohleförderung").compound shouldEqual 0.2732
    analyzer.polarityScores("als die Kohleförderung anstieg").compound shouldEqual 0.1779

    analyzer.polarityScores("Sentimentanalysen waren nie gut.").compound shouldEqual -0.3724

    analyzer.polarityScores("Die meisten Sentimentanalysen sind scheiße!").compound shouldEqual -0.5707

    analyzer.polarityScores("Sentimentanalysen waren noch nie so gut!").compound shouldEqual 0.7211 // original GerVADER:-0.5432
    analyzer.polarityScores("VADER ist mega.").compound shouldEqual 0.0

    analyzer.polarityScores("Ohne Zweifel, großartige Idee..").compound shouldEqual -0.1877

    analyzer.polarityScores("Roger Dodger is eine der verlockendsten Variationen des Themes.").compound shouldEqual 0.3182

    analyzer.polarityScores("Roger Dodger ist zumindest eine verlockende Variation des Themes.").compound shouldEqual 0.3182

    // original GerVADER: 0.3182, with leastCheck: -0.2411, with dampener "mitunter": -0.1889
    analyzer.polarityScores("Roger Dodger ist mitunter eine der wenigsten verlockenden Themen.").compound shouldEqual
    -0.1889

    val testnocaps = analyzer.polarityScores("Heute war ein schöner Tag")
    testnocaps.compound shouldEqual 0.5106
    val testWithoutEmoji = analyzer.polarityScores("Heute war ein SCHÖNER Tag")
    testWithoutEmoji.compound shouldEqual 0.6166

    //emoji tests
    analyzer.polarityScores("Heute war ein SCHÖNER Tag 😀").compound shouldEqual 0.7603
    analyzer.polarityScores("Heute war ein SCHÖNER Tag 💘 💋 😁").compound shouldEqual 0.9329
    analyzer.polarityScores("Heute war ein SCHÖNER Tag 🎉").compound shouldEqual 0.7739
    analyzer.polarityScores("Heute war ein SCHÖNER Tag 💣").compound shouldEqual 0.2103
    analyzer.polarityScores("Heute war ein SCHÖNER Tag 😃").compound shouldEqual 0.7603
    analyzer.polarityScores("Heute war ein SCHÖNER Tag 😄").compound shouldEqual 0.8602
    analyzer.polarityScores("Heute war ein SCHÖNER Tag 😅😆").compound shouldEqual 0.8415
    analyzer.polarityScores("Heute war ein SCHÖNER Tag 😆 😅").compound shouldEqual 0.8415

    val testallcaps = analyzer.polarityScores("HEUTE WAR EIN SCHÖNER TAG")
    testallcaps.compound shouldEqual 0.5106

    analyzer.polarityScores("es ist ein bisschen dumm").compound shouldEqual -0.5106

    analyzer.polarityScores("Nicht so krass letztlich.").compound shouldEqual -0.3713

    analyzer.polarityScores("Ohne Zweifel, eine exzellente Idee.").compound shouldEqual -0.2235

    analyzer.polarityScores("Anderseits, ist VADER ganz schön krass.").compound shouldEqual 0.7269

    analyzer.polarityScores("Andere Sentimentanalysentools können ziemlich schlecht sein.").compound shouldEqual -0.4033

    analyzer.polarityScores(" Mit Vader, sind Sentimentanalysen der Shit!").compound shouldEqual 0.0

    analyzer.polarityScores("Die meisten Sentimentanalysen sind scheiße!").compound shouldEqual -0.5707

    // test "nicht" at the end of sentence
    analyzer.polarityScores("ich mag dich").compound shouldEqual 0.4019
    analyzer.polarityScores("ich mag dich nicht").compound shouldEqual -0.3089 // normalize (1.7 x -0.74 = -1.258)

    // evaluation should be more accurate on sentence level
    analyzer.polarityScores("ich mag dich nicht. sie mag mich auch nicht.").compound shouldEqual -0.0842
    analyzer.polarityScoresSentenceLevel("ich mag dich nicht. sie mag mich auch nicht.") shouldEqual -0.3089
  }

  "A SentimentIntensityAnalyzer" should "negationCheck" in {
    val analyzer = new SentimentIntensityAnalyzer
    // 1word preceding lexicon word "schlechtes" (w/o stopwords)
    SentimentIntensityAnalyzer.negationCheck(1.0, Seq("Es", "ist", "kein", "schlechtes", "Buch", ":)"), 0, 3) shouldEqual -0.74
    // 2 word preceding lexicon word "angst" (w/o stopwords)
    SentimentIntensityAnalyzer.negationCheck(1.0, Seq("sie", "hat", "nicht", "wirklich","angst"), 1, 4) shouldEqual -0.74
    // 3 word preceding lexicon word (w/o stopwords)
    SentimentIntensityAnalyzer.negationCheck(1.0, Seq("Ich", "bin", "selten", "für", "sie", "da"), 2, 5) shouldEqual -0.74
    // 3 word preceding the lexikon, word preceding the lexikon is "this", "so"
    SentimentIntensityAnalyzer.negationCheck(1.0, Seq("nie", "wieder", "so",":-)"), 2, 3) shouldEqual 1.25
    // 2 words preceding the lexikon is "never this/so" -> "nie so"
    SentimentIntensityAnalyzer.negationCheck(1.0, Seq("Ich", "war", "nie", "so", "dankbar", "ein", "freundliches", "gesicht", "zu", "sehen"), 1, 4) shouldEqual 1.25
    // 3 words preceding the lexikon is "never this/so" -> "nie so"
    SentimentIntensityAnalyzer.negationCheck(1.0, Seq("Ich", "war", "noch", "nie", "so", "krass", "hungrig"), 2, 6) shouldEqual 1.25
    SentimentIntensityAnalyzer.negationCheck(1.0, Seq("ich", "mag", "dich"), 0, 1) shouldEqual 1.0

    // negation 4 words preceding the item at i has no influence
    SentimentIntensityAnalyzer.negationCheck(1.0, Seq("ich", "mochte", "Mathe", "nicht", "damals", "doch", "jetzt", "mag", "ich", "es"), 4, 7) shouldEqual 1.0

    SentimentIntensityAnalyzer.negationCheck(2.4, Seq("Ich", "hab", "selten", "so", "gelacht"), 2, 4) shouldEqual 3.0
    // 2.4*1.25

    // negation should not influence words of following sentence
    SentimentIntensityAnalyzer.negationCheck(2.4, Seq("Sie", "mag", "den", "Film", "nicht", "sie", "hat", "trotzdem", "gelacht"), 3, 8, sentenceLevel = false) shouldEqual 2.4
    SentimentIntensityAnalyzer.negationCheck(2.4, Seq("Sie", "mag", "den", "Film", "nicht", "sie", "hat", "trotzdem", "gelacht"), 3, 8, sentenceLevel = true) shouldEqual -1.776
  }

  "A SentimentIntensityAnalyzer" should "butCheck" in {
    SentimentIntensityAnalyzer.butCheck(Seq("nicht", "schlecht", "aber", "auch", "nicht", "gut"), ListBuffer(1, 1, 0, 1, 1, 1)) shouldEqual ListBuffer(0.5, 0.5, 0, 1.5, 1.5, 1.5)
    SentimentIntensityAnalyzer.butCheck(Seq("nicht", "schlecht", "ABER", "ohne", "Zweifel", "unkreativ"), ListBuffer(1, 1, 0, 1, 1, 1, 1)) shouldEqual ListBuffer(0.5, 0.5, 0, 1.5, 1.5, 1.5, 1.5)
    SentimentIntensityAnalyzer.butCheck(Seq("nicht", "schlecht", "aBeR", "ohne", "Zweifel", "dumm"), ListBuffer(1, 1, 0, 1, 1, 1, 1)) shouldEqual ListBuffer(0.5, 0.5, 0, 1.5, 1.5, 1.5, 1.5)

    // no contrast conjuction
    SentimentIntensityAnalyzer.butCheck(Seq("Ich", "kann", "das", "nicht",":-)"), ListBuffer(1, 1, 1, 1, 1)) shouldEqual ListBuffer(1, 1, 1, 1, 1)
  }

  "A SentimentIntensityAnalyzer" should "leastcheck" in {
    val analyzer = new SentimentIntensityAnalyzer()

    val valence = 1
    analyzer.leastCheck(valence, Seq("du", "bist", "der", "am", "wenigsten", "schlaue", "mensch", "den", "ich", "kenne"), 5) shouldEqual -0.74
    analyzer.leastCheck(valence, Seq("am", "wenigsten", "schlau"), 2) shouldEqual -0.74
    analyzer.leastCheck(valence, Seq("wenigsten", "Ängste"), 1) shouldEqual -0.74


    // at least/very least preceding word at given index doesn't modify valence -> "Zumindest"
    analyzer.leastCheck(valence, Seq("Zumindest", "mag", "sie", "dich"), 2) shouldEqual 1.0
    analyzer.leastCheck(valence, Seq("Immerhin", "bist", "du", "schlau"), 3) shouldEqual 1.0
  }

  "A SentimentIntensityAnalyzer" should "idiomsCheck" in {
    SentimentIntensityAnalyzer.idiomsCheck(0.0, Seq("VADEr", "is", "der", "scheiß"), 3) shouldEqual 3

   //SentimentIntensityAnalyzer.idiomsCheck(0.0, Seq("VADER", "is", "the", "bomb"), 0) shouldEqual 3
    // booster/dampener check
    //SentimentIntensityAnalyzer.idiomsCheck(0.0, Seq("The", "book", "was", "kind", "of", "good"), 5) shouldEqual -0.293
    //SentimentIntensityAnalyzer.idiomsCheck(0.0, Seq("The", "book", "was", "sort", "of", "good"), 5) shouldEqual -0.293
    // word at index 4 of
    SentimentIntensityAnalyzer.idiomsCheck(0.0, Seq("The", "book", "was", "sort", "of", "good"), 4) shouldEqual 0

    // zeroOne idioms
    SentimentIntensityAnalyzer.idiomsCheck(0.0, Seq("The", "VADER", "algorithm", "ist", "the", "shit", "ha"), 4) shouldEqual 3

    // zeroOneTwo
    SentimentIntensityAnalyzer.idiomsCheck(0.0, Seq("The", "VADER", "algorithm", "will", "cut", "the", "mustard"), 4)shouldEqual 2
    // SentimentIntensityAnalyzer.idiomsCheck(0.0, Seq("was", "ein", "schuss", "ins", "knie"), 4) shouldEqual -2.3

    // index out of bounds
    SentimentIntensityAnalyzer.idiomsCheck(0.0, Seq("VADER", "will", "cut", "the", "mustard"), 2) shouldEqual 0.0
    SentimentIntensityAnalyzer.idiomsCheck(0.0, Seq("VADER", "will", "cut", "the", "mustard"), 5) shouldEqual 0.0

    // check for dampener in front of idiom
    SentimentIntensityAnalyzer.idiomsCheck(0.0, Seq("VADEr", "is", "schon", "ein", "bisschen", "der", "scheiß"), 5) shouldEqual 2.707
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

    analyzer.scoreValence(Seq(), "") shouldEqual SentimentAnalysisResults()
  }

  "A SentimentIntensityAnalyzer" should "sentimentValence" in {
    val analyzer = new SentimentIntensityAnalyzer()
    // lexikon does not contain item
    analyzer.sentimentValence(1.0, new SentiText("Es ist kein schlechtes Buch"), "Buch", 4) shouldEqual 1.0
    // lexikon contains item never check
    analyzer.sentimentValence(1.0, new SentiText("Es ist kein schlechtes Buch"), "schlechtes", 3) shouldEqual 1.48 // schlechtes=-2.0  negationCheck -0.74*-2.0
    // check if sentiment laden word is in ALL CAPS (while others aren't)
    analyzer.sentimentValence(0.0, new SentiText("Es ist ein SCHLECHTES Buch"), "SCHLECHTES", 3) shouldEqual -2.733
    // (-2.0-0.733)
    analyzer.sentimentValence(0.0, new SentiText("Es ist kein extrem schlechtes Buch"), "schlechtes", 4) shouldEqual 1.69682

    // dampen scalar of booster by 0.95 if 2 words preceding is booster
    // extrem = 0.293, Anstieg = 1.1
    // (0.293*0.95) + 1.1 = 1.37835
    analyzer.sentimentValence(0.0, new SentiText("Es ist ein extrem steiler Anstieg"), "Anstieg", 5) shouldEqual (0.293*0.95)+1.1
    // dampen scalar of booster by 0.9 if 3 words preceding is booster
    analyzer.sentimentValence(0.0, new SentiText("ich bin total fertig und traurig"), "traurig", 5) shouldEqual (-0.293*0.9)-2.2
    // failing because "komplett is in lexicon"
    // analyzer.sentimentValence(0.0, new SentiText("ich bin komplett fertig und traurig"), "traurig", 5) shouldEqual -2.4637
  }

  "A SentimentIntensityAnalyzer" should "makeLexDict" in {
    val analyzer = new SentimentIntensityAnalyzer()
    val lexikon = analyzer.makeLexDict()
    lexikon("$:") shouldEqual -1.5
  }

  "A SentimentIntensityAnalyzer" should "makeEmojiDict" in {
    val analyzer = new SentimentIntensityAnalyzer()
    val emojiLexikon = analyzer.makeEmojiDict()
    emojiLexikon("💔") shouldEqual "broken heart"
    emojiLexikon("🤡") shouldEqual "clown face"
    emojiLexikon("🤣") shouldEqual "rolling on the floor laughing"
    emojiLexikon("😂") shouldEqual "face with tears of joy"
  }

   "A SentimentIntensityAnalyzer" should "calculate polarityScores on sentence Level" in {
    val analyzer = new SentimentIntensityAnalyzer()
    analyzer.polarityScoresSentenceLevel("Fußball entfacht in mir ein Feuer der Leidenschaft. GerVADER hat viel Potential <3") shouldEqual  0.4404
    analyzer.polarityScoresSentenceLevel("Die meisten Sentimentanalysen sind scheiße! Sentimentanalysen waren noch nie so gut!") shouldEqual  0.0752
    // look at example of Tymann et. al
    analyzer.polarityScoresSentenceLevel("Ich finde, dass diese Menschen wirklich freundlich sind.") shouldEqual  0.4588
    analyzer.polarityScoresSentenceLevel("Ich finde nicht, dass diese Menschen wirklich freundlich sind.") shouldEqual  -0.357
  }

  "A SentimentIntensityAnalyzer" should "negate sentence if 'nicht' is following the lexicon word" in {
    val analyzer = new SentimentIntensityAnalyzer()
    SentimentIntensityAnalyzer.negationCheck(1.0, Seq("Ich ", "mag", "das"),  -1, 2) shouldEqual 1.0
    // test of negation word is 1 word following the lexicon word
    SentimentIntensityAnalyzer.negationCheck(1.4, Seq("es ", "funktioniert", "nicht"),  -2, 1) shouldEqual -1.036
    // examples given by Tymann et. al
    // negation word is 2 words following the lexicon word
    SentimentIntensityAnalyzer.negationCheck(1.0, Seq("Ich ", "mag", "das", "nicht"),  -3, 1) shouldEqual -0.74
    // negation word is 3 words following the lexicon word
    SentimentIntensityAnalyzer.negationCheck(1.0, Seq("Ich ", "mochte", "das", "noch", "nie"),  -4, 1) shouldEqual -0.74
    // negation word is 4 words following the lexicon word
    SentimentIntensityAnalyzer.negationCheck(1.0, Seq("Ich ", "mochte", "das", "wirkich", "noch", "nie"),  -5, 1) shouldEqual 1.0
  }

  "A SentimentIntensityAnalyzer" should "handle problems with  negation of longer sentences" in {
    // Tymann et. al 1. Ich finde nicht, dass diese Menschen wirklich freundlich sind. (rated posi- tive, should be negative)
    val analyzer = new SentimentIntensityAnalyzer()
    SentimentIntensityAnalyzer.negationCheck(1.0, Seq("Ich ", "finde", "nicht", "dass", "diese", "Menschen", "wirklich", "freundlich", "sind"),  4, 7) shouldEqual -0.74
  }
}
