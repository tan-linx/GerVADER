
package sentiment

import sentiment.utils.{ResourceUtils, SentimentUtils}

import scala.collection.mutable.ListBuffer
import scala.collection.{Seq, _}
import scala.util.control.Breaks._

/**
 * An abstraction to represent the sentiment intensity analyzer.
 */
class SentimentIntensityAnalyzer {
  val VADER_LEXICON_PATH = "/vader_lexicon.txt"

  val ExclIncr: Double = 0.292
  val QuesIncrSmall: Double = 0.18
  val QuesIncrLarge: Double = 0.96

  val lexiconFile: Seq[String] = ResourceUtils.readFileAsList(VADER_LEXICON_PATH)
  var lexicon: Map[String, Double] = makeLexDict()

  private case class SiftSentiments(var posSum: Double = 0, var negSum: Double = 0, var neuCount: Int = 0)

  /**
   *
   * Makes Lex dict
   *
   * @return
   */
  def makeLexDict(): Map[String, Double] = {
    lexiconFile.map(line => {
        val lineArray = line.trim().split('\t')
        (lineArray(0) -> lineArray(1).toDouble)
      }
    ).toMap
  }

  /**
   * Return metrics for positive, negative and neutral sentiment based on the input text.
   *
   * @param input
   * @return
   */
  def polarityScores(input: String): SentimentAnalysisResults = {
    // todo: convert emojis to their textual descriptions

    val sentiText: SentiText = new SentiText(input)
    var sentiments: ListBuffer[Double] = ListBuffer[Double]()

    val wordsAndEmoticons: Seq[String] = sentiText.wordsAndEmoticons

    breakable {
      for ((item, i) <- wordsAndEmoticons.view.zipWithIndex) {
        // append valence 0 if word is not in lexikon
        var valence: Double = 0
        // check for vader_lexicon words that may be used as modifiers or negations and turn their valence to 0
        if ((i < wordsAndEmoticons.size - 1 &&
             item.toLowerCase() == "kind" &&
             wordsAndEmoticons(i + 1) == "of")
          || SentimentUtils.boosterDict.contains(item.toLowerCase())) {
          sentiments += valence
          break()
        }

        val (_valence, _sentiments) = sentimentValence(valence, sentiText, item, i, sentiments)
        valence = _valence
        sentiments = _sentiments
      }
    }

    /* val (_valence, _sentiments) = valenceAndSentiments(wordsAndEmoticons.view.zipWithIndex.toList, sentiText, sentiments)
    sentiments = _sentiments */
    sentiments = butCheck(wordsAndEmoticons, sentiments)

    scoreValence(sentiments, input)
  }

  /**
   * Return metrics for positive, negative and neutral sentiment based on the input text.
   *
   * @param valenc valence of word
   * @param sentiText text that contains item
   * @param item item that sentiment valence is being calculated for
   * @param i index of item in sentiText
   * @param sentiments already calculated sentiments of text
   * @return item's valence and sentiments list appended with items valence
   */
  def sentimentValence(valenc: Double, sentiText: SentiText,
    item: String, i: Int, sentiments: ListBuffer[Double]): (Double, ListBuffer[Double]) = {

    var valence = valenc
    val itemLowerCase: String = item.toLowerCase()
    if (!lexicon.contains(itemLowerCase)) {
      sentiments += valence
      return (valence, sentiments)
    } //check

    val isCapDiff: Boolean = sentiText.isCapDifferential
    val wordsAndEmoticons = sentiText.wordsAndEmoticons
    valence = lexicon(itemLowerCase)

    if (isCapDiff && SentimentUtils.isUpper(item)) {
      if (valence > 0) {
        valence += SentimentUtils.CIncr
      } else {
        valence -= SentimentUtils.CIncr
      }
    }

    for (startI <- 0 until 3) {
      if (i > startI && !lexicon.contains(wordsAndEmoticons(i - (startI + 1)).toLowerCase())) {
        var s: Double = SentimentUtils.scalarIncDec(wordsAndEmoticons(i - (startI + 1)), valence, isCapDiff)

        if (startI == 1 && s != 0) {
          s = s * 0.95
        }

        if (startI == 2 && s != 0) {
          s = s * 0.9
        }

        valence = valence + s

        valence = neverCheck(valence, wordsAndEmoticons, startI, i)

        if (startI == 2) {
          valence = idiomsCheck(valence, wordsAndEmoticons, i)
        }

      }
    }

    valence = leastCheck(valence, wordsAndEmoticons, i)
    sentiments += valence

    (valence, sentiments)
  }

  def scoreValence(sentiments: Seq[Double], text: String): SentimentAnalysisResults = {

    if (sentiments.isEmpty) {
      return SentimentAnalysisResults() //will return with all 0
    }

    var sum: Double = sentiments.sum
    var puncAmplifier: Double = punctuationEmphasis(text)

    // add emphasis for puncutation
    sum += scala.math.signum(sum) * puncAmplifier

    val compound: Double = SentimentUtils.normalize(sum)
    val sifted: SiftSentiments = siftSentimentScores(sentiments)

    if (sifted.posSum > scala.math.abs(sifted.negSum)) {
      sifted.posSum += puncAmplifier
    } else if (sifted.posSum < scala.math.abs(sifted.negSum)) {
      sifted.negSum -= puncAmplifier
    }

    val total: Double = sifted.posSum + scala.math.abs(sifted.negSum) + sifted.neuCount

    SentimentAnalysisResults(
      compound = roundWithDecimalPlaces(compound, 4),
      positive = roundWithDecimalPlaces(scala.math.abs(sifted.posSum / total), 3),
      negative = roundWithDecimalPlaces(scala.math.abs(sifted.negSum / total), 3),
      neutral = roundWithDecimalPlaces(scala.math.abs(sifted.neuCount / total), 3)
    )
  }

  def idiomsCheck(valenc: Double, wordsAndEmoticons: Seq[String], i: Int): Double = {
    // todo: check negative indices

    var valence = valenc
    val oneZero = wordsAndEmoticons(i - 1).concat(" ").concat(wordsAndEmoticons(i))
    val twoOneZero = wordsAndEmoticons(i - 2)
      .concat(" ").concat(wordsAndEmoticons(i - 1)).concat(" ").concat(wordsAndEmoticons(i))
    val twoOne = wordsAndEmoticons(i - 2)
      .concat(" ").concat(wordsAndEmoticons(i - 1))
    val threeTwoOne = wordsAndEmoticons(i - 3)
      .concat(" ").concat(wordsAndEmoticons(i - 2)).concat(" ").concat(wordsAndEmoticons(i - 1))
    val threeTwo = wordsAndEmoticons(i - 3).concat(" ").concat(wordsAndEmoticons(i - 2))

    val sequences = Array(oneZero, twoOneZero, twoOne, threeTwoOne, threeTwo)

    breakable {
      for (seq <- sequences) {
        if (SentimentUtils.specialCaseIdioms.contains(seq)) {
          valence = SentimentUtils.specialCaseIdioms(seq)
          break()
        }
      }
    }

    if (wordsAndEmoticons.size - 1 > i) {
      val zeroOne = wordsAndEmoticons(i).concat(" ").concat(wordsAndEmoticons(i + 1))
      if (SentimentUtils.specialCaseIdioms.contains(zeroOne)) {
        valence = SentimentUtils.specialCaseIdioms(zeroOne)
      }
    }
    if (wordsAndEmoticons.size - 1 > i + 1) {
      val zeroOneTwo = wordsAndEmoticons(i)
        .concat(" ").concat(wordsAndEmoticons(i + 1)).concat(" ").concat(wordsAndEmoticons(i + 2))
      if (SentimentUtils.specialCaseIdioms.contains(zeroOneTwo)) {
        valence = SentimentUtils.specialCaseIdioms(zeroOneTwo)
      }
    }

    // check for booster/dampener bi-grams such as 'sort of' or 'kind of'
    val potentialBooster = Array(threeTwoOne, threeTwo, twoOne)
    checkBooster(potentialBooster, valence)
  }

  def leastCheck(valenc: Double, wordsAndEmoticons: Seq[String], i: Int): Double = {

    var valence: Double = valenc

    if (i > 1 && !lexicon.contains(wordsAndEmoticons(i - 1).toLowerCase()) &&
      wordsAndEmoticons(i - 1).toLowerCase() == "least") {
      if (wordsAndEmoticons(i - 2).toLowerCase() != "at" && wordsAndEmoticons(i - 2).toLowerCase() != "very") {
        valence = valence * SentimentUtils.NScalar
      }
    } else if (i > 0 && !lexicon.contains(wordsAndEmoticons(i - 1).toLowerCase())
      && wordsAndEmoticons(i - 1).toLowerCase() == "least") {
      valence = valence * SentimentUtils.NScalar
    }

    valence
  }

  def neverCheck(valenc: Double, wordsAndEmoticons: Seq[String], startI: Int, i: Int): Double = {
    val wordsAndEmoticonsLower = wordsAndEmoticons.map(_.toLowerCase())

    var valence = valenc
    startI match {
      case 0 => {
        val list = List(wordsAndEmoticonsLower(i - 1)) // 1 word preceding lexicon word (w/o stopwords)
        if (SentimentUtils.negated(list)) {
          valence = valence * SentimentUtils.NScalar
        }
      }
      case 1 => {
        if (wordsAndEmoticonsLower(i - 2) == "never" &&
          (wordsAndEmoticonsLower(i - 1) == "so" || wordsAndEmoticonsLower(i - 1) == "this")) {
          valence = valence * 1.25
        } else if (SentimentUtils.negated(List(wordsAndEmoticonsLower(i - (startI + 1))))) { // 2 words preceding the lexicon word position
          valence = valence * SentimentUtils.NScalar
        }
      }
      case 2 => {
        if (wordsAndEmoticonsLower(i - 3) == "never"
          && (wordsAndEmoticonsLower(i - 2) == "so" || wordsAndEmoticonsLower(i - 2) == "this")
          || (wordsAndEmoticonsLower(i - 1) == "so" || wordsAndEmoticonsLower(i - 1) == "this")) {
          valence = valence * 1.25
        } else if (SentimentUtils.negated(List(wordsAndEmoticonsLower(i - (startI + 1))))) { //  3 words preceding the lexicon word position
          valence = valence * SentimentUtils.NScalar
        }
      }
    }
    valence
  }

  def butCheck(wordsAndEmoticons: Seq[String], sentiments: ListBuffer[Double]): ListBuffer[Double] = {
    val wordsAndEmoticonsLower = wordsAndEmoticons.map(_.toLowerCase())

    if (!wordsAndEmoticonsLower.contains("but")) {
      return sentiments
    }

    val butIndex: Int = wordsAndEmoticonsLower.indexOf("but")

    val result = sentiments.view.zipWithIndex.map((sentiment: Double, i: Int) => {
      if (i < butIndex) sentiment * 0.5
      else if (i > butIndex) sentiment * 1.5
      else sentiment
    })
    result.toList.to(ListBuffer)
  }

  private def checkBooster(potentialBooster: Array[String], valence: Double): Double = {
    val word = potentialBooster.head
    if (SentimentUtils.boosterDict.contains(word)) valence+SentimentUtils.boosterDict.getOrElse(word, 0.0)
    else if (potentialBooster.tail.size == 0) valence
    else checkBooster(potentialBooster.tail, valence)
  }

  /**
    * Add emphasis from exclamation points and question marks
    *
    * @param text
    * @return
    */
  private def punctuationEmphasis(text: String): Double = {
    amplifyExclamation(text) + amplifyQuestion(text)
  }

  private def amplifyExclamation(text: String): Double = {
    // check for added emphasis resulting from exclamation points (up to 4 of them)
    var epCount: Int = text.count(x => x == '!')

    if (epCount > 4) {
      epCount = 4
    }

    // (empirically derived mean sentiment intensity rating increase for
    // exclamation points)
    epCount * ExclIncr
  }

  /**
    * check for added emphasis resulting from question marks (2 or 3+)
    *
    * @param text
    * @return
    */
  private def amplifyQuestion(text: String): Double = {
    val qmCount: Int = text.count(x => x == '?')

    if (qmCount < 2) return 0 // no or 1
    else if (qmCount <= 3) { // 2 or 3
      return qmCount * QuesIncrSmall
    } else { // 3+ question marks
       QuesIncrLarge
    }
  }

  /**
   * separate positive versus negative sentiment scores
   * @param sentiments
   * @return
   */
  private def siftSentimentScores(sentiments: Seq[Double]): SiftSentiments = {
    val siftSentiments = SiftSentiments()

    for (sentiment <- sentiments) {
      if (sentiment > 0) {
        siftSentiments.posSum += (sentiment + 1); // compensates for neutral words that are counted as 1
      }

      if (sentiment < 0) {
        siftSentiments.negSum += (sentiment - 1)
      }

      if (sentiment == 0) {
        siftSentiments.neuCount += 1
      }
    }

    siftSentiments
  }

  private def roundWithDecimalPlaces(value: Double, decPlaces: Int): Double = {
    decPlaces match {
      case 1 => (value * 10).round / 10.toDouble
      case 2 => (value * 100).round / 100.toDouble
      case 3 => (value * 1000).round / 1000.toDouble
      case 4 => (value * 10000).round / 10000.toDouble
    }
  }

  private def valenceAndSentiments(
    wordsAndEmoticons: List[(String, Int)],
    sentiText: SentiText,
    _sentiments: ListBuffer[Double]): (Double, ListBuffer[Double]) = {
    var sentiments = _sentiments
    val item = wordsAndEmoticons.head._1
    val i = wordsAndEmoticons.head._2
    var valence: Double = 0
    // check for vader_lexicon words that may be used as modifiers or negations and turn their valence to 0
    if ((i < wordsAndEmoticons.size - 1 &&
          item.toLowerCase() == "kind" &&
          wordsAndEmoticons(i + 1)._1 == "of")
      || SentimentUtils.boosterDict.contains(item.toLowerCase())) {
      sentiments += valence
    } else {
      val (_valence, _sentiments) = sentimentValence(valence, sentiText, item, i, sentiments)
      valence = _valence
      sentiments = _sentiments
    }

    if (wordsAndEmoticons.tail == List.empty) (valence, sentiments)
    else valenceAndSentiments(wordsAndEmoticons.tail, sentiText, sentiments)
  }
}
