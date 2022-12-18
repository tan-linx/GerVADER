
package sentiment

import sentiment.utils.{ResourceUtils, SentimentUtils}
import java.nio.charset.StandardCharsets

import scala.collection.mutable.ListBuffer
import scala.collection.{Seq, _}
import scala.util.control.Breaks._

/**
 * An abstraction to represent the sentiment intensity analyzer.
 */
class SentimentIntensityAnalyzer {
  private val VADER_LEXICON_PATH = "gervader_lexikon.txt"
  private val VADER_EMOJI_LEXICON_PATH = "emoji_utf8_lexicon.txt"

  private val lexicon: Map[String, Double] = makeLexDict()
  private val emojiLexikon: Map[String, String] = makeEmojiDict()

  private case class SiftSentiments(var posSum: Double = 0, var negSum: Double = 0, var neuCount: Int = 0)

  /**
   * Computes sentiment valence of `input`
   *
   * @param input text that polarity scores are being calculated for
   * @return polarity scores of `input`
   */
  def polarityScores(input: String): SentimentAnalysisResults = {
    // convert emojis to their textual descriptions
    val emojisInInput = "[^\u0000-\uFFFF]".r.findAllIn(input).toList
    val textNoEmoji = replaceEmojisWithDescription(emojisInInput, input)

    // tokenize
    val sentiText: SentiText = new SentiText(textNoEmoji.trim())
    val wordsAndEmoticons: Seq[String] = sentiText.wordsAndEmoticons
    val valence: Double = 0

    val sentimentsBeforeButCheck = wordsAndEmoticons.view.zipWithIndex.map(
      (item, i) => {
        if ((i < wordsAndEmoticons.size-1 &&
            item.toLowerCase() == "kind" &&
            wordsAndEmoticons(i + 1).toLowerCase() == "of")
            || SentimentUtils.boosterDict.contains(item.toLowerCase())) {
          valence
        } else {
          sentimentValence(valence, sentiText, item, i)
        }
      }
    ).toList.to(ListBuffer)

    val sentiments = butCheck(wordsAndEmoticons, sentimentsBeforeButCheck)
    scoreValence(sentiments, input)
  }

  /**
   * Computes sentiment valence of `item` at index `i` in given `sentiText`
   *
   * @param valenc `valence` of word before sentiment analysis
   * @param sentiText input text
   * @param item `item` that sentiment valence is being calculated for
   * @param i `index` of `item` in sentiText
   * @return valence of `item`
  */
  def sentimentValence(valenc: Double, sentiText: SentiText, item: String, i: Int): Double = {
    var valence = valenc
    val isCapDiff: Boolean = sentiText.isCapDifferential
    val wordsAndEmoticons = sentiText.wordsAndEmoticons
    val itemLowerCase: String = item.toLowerCase()
    val itemFirstLetterCapitalized = item.toLowerCase().capitalize

    if (!lexicon.contains(item)
        && !lexicon.contains(itemLowerCase)
        && !lexicon.contains(itemFirstLetterCapitalized)) {
      return valence
    }

    //1. Check if the currently inspected word can be found in the lexicon
    //2. If not, transform the word to all lower cases and recheck the lexicon
    //3. If not, only capitalize the first letter of the word and recheck the lexicon
    val words = List(item,itemLowerCase, itemFirstLetterCapitalized)
    def getValence(items: List[String]): Double = {
      val word = items.head
      if (lexicon.contains(word))
        lexicon(word)
      else getValence(items.tail)
    }
    valence = getValence(words)

    //   # check for "no" as negation for an adjacent lexicon item vs "no" as its own stand-alone lexicon item
    // if (itemLowerCase == "no" &&
    //     i != wordsAndEmoticons.size-1 &&
    //     lexicon.contains(wordsAndEmoticons(i+1).toLowerCase)) {
    //   // don't use valence of "no" as a lexicon item. Instead set it's valence to 0.0 and negate the next item
    //   valence = 0.0
    //   if ((i > 0 && wordsAndEmoticons(i - 1).toLowerCase == "no") ||
    //       (i > 1 && wordsAndEmoticons(i - 2).toLowerCase == "no") ||
    //       (i > 2 && wordsAndEmoticons(i - 3).toLowerCase == "no"
    //       && List("or", "nor").contains(wordsAndEmoticons(i - 1).toLowerCase))
    //   ) then valence = valence * SentimentUtils.NScalar
    // }

    // check if sentiment laden word is in ALL CAPS (while others aren't)
    if (isCapDiff && SentimentUtils.isUpper(item)) {
      if (valence > 0) {
        valence += SentimentUtils.CIncr
      } else {
        valence -= SentimentUtils.CIncr
      }
    }

    // dampen valence, negationCheck & idiomsCheck
    valence = (0 until 3).foldLeft(valence)(
        (valenceAcc, startI) => {
        if (i > startI && (!lexicon.contains(wordsAndEmoticons(i - (startI + 1)).toLowerCase()))
          ) {
          val s: Double = SentimentUtils.scalarIncDec(wordsAndEmoticons(i - (startI + 1)), valenceAcc, isCapDiff) //
          val scalar =
            if (startI == 1 && s != 0) s * 0.95 // 2 words preceding item is booster
            else if (startI == 2 && s != 0) s * 0.9 // 3 words preceding item is booster
            else s
          val valenceNeg = neverCheck(valenceAcc + scalar, wordsAndEmoticons, startI, i)
          if (startI == 2) { // ensures there are at least 2 preceding words
             idiomsCheck(valenceNeg, wordsAndEmoticons, i)
          } else {
             valenceNeg
          }
        } else {
          valenceAcc
        }
      }
    )

    leastCheck(valence, wordsAndEmoticons, i)
  }

  /**
   * Scores sentiment valence for given `text`
   * @param sentiments sentiments of tokens in `text`
   * @param text `text` that sentiment is being calculated for
   * @return weighted positive, negative & neutral and compound sentiment valence of text
   */
  def scoreValence(sentiments: Seq[Double], text: String): SentimentAnalysisResults = {
    if (sentiments.isEmpty) {
      return SentimentAnalysisResults() //will return with all 0
    }

    val sum: Double = sentiments.sum
    val puncAmplifier: Double = punctuationEmphasis(text)

    // compute and add emphasis from punctuation in text
    val compound: Double = SentimentUtils.normalize(sum + scala.math.signum(sum) * puncAmplifier)
    // discriminate between positive, negative and neutral sentiment scores
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

  /**
   * Checks for special idioms e.g. "the shit"
   * @param wordsAndEmoticons `tokens` of text to analyze
   * @param valenc `valence` of word
   * @param i `index` of word in `wordsAndEmoticons` that is being viewed currently
   * @return `valence` of word at index `i` after idiomsCheck
   */
  def idiomsCheck(valenc: Double, wordsAndEmoticons: Seq[String], i: Int): Double = {
    if (i < 3 || i > wordsAndEmoticons.size - 1) return valenc

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

    // check if there is an idiom like "the shit" in sequences
    def containsSpecialCaseIdiomHelper(valence: Double, sequences: Array[String]): Double = {
      if (sequences.isEmpty) return valence
      if (SentimentUtils.specialCaseIdioms.contains(sequences.head))
        SentimentUtils.specialCaseIdioms(sequences.head)
      else
        containsSpecialCaseIdiomHelper(valence, sequences.tail)
    }
    valence = containsSpecialCaseIdiomHelper(valence, sequences)

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
    def checkBooster(potentialBooster: Array[String], valence: Double): Double = {
      val word = potentialBooster.head
      if (SentimentUtils.boosterDict.contains(word)) valence + SentimentUtils.boosterDict.getOrElse(word, 0.0)
      else if (potentialBooster.tail.size == 0) valence
      else checkBooster(potentialBooster.tail, valence)
    }

    checkBooster(potentialBooster, valence)
  }

  /**
   * Checks for negation case using "least" (german: "wenigsten").
   * Does not modify valence of word following "at least" or "very least" ("wenigstens").
   * @param valence valence of token at index `i` in `wordsAndEmoticons` before leastCheck
   * @param wordsAndEmoticons tokens of text that is being analyzed
   * @param i index of token in `wordsAndEmoticons` that is being analyzed
   * @return valence of token at index `i` in `wordsAndEmoticons` after leastCheck
   */
  def leastCheck(valence: Double, wordsAndEmoticons: Seq[String], i: Int): Double = {
    if (i > 1 && !lexicon.contains(wordsAndEmoticons(i - 1).toLowerCase()) &&
      (wordsAndEmoticons(i - 1).toLowerCase() == "wenigsten")) {
      if (wordsAndEmoticons(i - 2).toLowerCase() != "at" && wordsAndEmoticons(i - 2).toLowerCase() != "very")
        return valence * SentimentUtils.NScalar
    } else if (i > 0 && !lexicon.contains(wordsAndEmoticons(i - 1).toLowerCase())
      && (wordsAndEmoticons(i - 1).toLowerCase() == "wenigsten"))
        return valence * SentimentUtils.NScalar
    valence
  }

  /**
   * Checks for negations in sentence e.g. "nicht"
   * @param valence valence of token at index `i` in `wordsAndEmoticons` before negationCheck
   * @param wordsAndEmoticons tokens of text that is being analyzed
   * @param startI determines how many preceding words are being looked at, e.g. 1, 2, 3
   * @param i index of token in `wordsAndEmoticons` that is being analyzed
   * @return valence of token at index `i` in `wordsAndEmoticons` after negationCheck
  */
  def neverCheck(valence: Double, wordsAndEmoticons: Seq[String], startI: Int, i: Int): Double = {
    val wordsAndEmoticonsLower = wordsAndEmoticons.map(_.toLowerCase())
    startI match {
      case 0 => {
        val list = List(wordsAndEmoticonsLower(i - 1)) // 1 word preceding lexicon word (w/o stopwords)
        if (SentimentUtils.negated(list)) return valence * SentimentUtils.NScalar
      }
      case 1 => {
        if (wordsAndEmoticonsLower(i - 2) == "nie" &&
            (wordsAndEmoticonsLower(i - 1) == "so" || wordsAndEmoticonsLower(i - 1) == "this"))
          return valence * 1.25
        else if (SentimentUtils.negated(List(wordsAndEmoticonsLower(i - (startI + 1))))) // words preceding the lexicon word position
          return valence * SentimentUtils.NScalar
      }
      case 2 => {
        if (wordsAndEmoticonsLower(i - 3) == "nie" &&
            (wordsAndEmoticonsLower(i - 2) == "so" || wordsAndEmoticonsLower(i - 2) == "this") || (wordsAndEmoticonsLower(i - 1) == "so" || wordsAndEmoticonsLower(i - 1) == "this"))
          return valence * 1.25
        else if (SentimentUtils.negated(List(wordsAndEmoticonsLower(i - (startI + 1))))) // 3 words preceding the lexicon word position
          return valence * SentimentUtils.NScalar
      }
      case _ => valence
    }
    valence
  }

  /**
   * Checks for modification in sentiment due to contrastive conjunction 'but'.
   * Words before contrastive conjunction are weighted with 0.5, where as words after with 1.5.
   *
   * @param wordsAndEmoticons tokens of text that is being analyzed
   * @param sentiments sentiments of each token in `wordsAndEmoticons`
   * @return sentiments of `wordsAndEmoticons` after checking for contrastive conjunction
   */
  def butCheck(wordsAndEmoticons: Seq[String], sentiments: ListBuffer[Double]): ListBuffer[Double] = {
    val wordsAndEmoticonsLower = wordsAndEmoticons.map(_.toLowerCase())

    if (!wordsAndEmoticonsLower.contains("aber")) return sentiments

    val butIndex: Int = wordsAndEmoticonsLower.indexOf("aber")

    sentiments.view.zipWithIndex.map((sentiment: Double, i: Int) => {
      if (i < butIndex) sentiment * 0.5
      else if (i > butIndex) sentiment * 1.5
      else sentiment
    }).toList.to(ListBuffer)
  }

  /**
    * Adds emphasis from exclamation points (up to 4 of them) and question marks (2 or 3+)
    *
    * @param text text which is being analyzed for exclamation points and question marks
    * @return Added sentiment points from exclamation points and question marks.
    */
  private def punctuationEmphasis(text: String): Double = {
    amplifyExclamation(text) + amplifyQuestion(text)
  }

  /**
    * Checks for added emphasis resulting from exclamation points (up to 4 of them)
    *
    * @param text text which is being analyzed for exclamation points
    * @return added sentiment points from exclamation points
    */
  private def amplifyExclamation(text: String): Double = {
    // (empirically derived mean sentiment intensity rating increase for exclamation points)
    val ExclIncr: Double = 0.292
    val epCount: Int = text.count(x => x == '!')

    if (epCount > 4) 4 * ExclIncr
    else epCount * ExclIncr
  }

  /**
    * Checks for added emphasis resulting from question marks (2 or 3+)
    *
    * @param text text which is being analyzed for `question marks`
    * @return added sentiment points from question marks
    */
  private def amplifyQuestion(text: String): Double = {
    val QuesIncrSmall: Double = 0.18
    val QuesIncrLarge: Double = 0.96
    val qmCount: Int = text.count(x => x == '?')

    if (qmCount < 2) 0 // no or 1
    else if (qmCount <= 3) qmCount * QuesIncrSmall // 2 or 3
    else QuesIncrLarge // 3+ question marks
  }

  /**
   * Separates positive versus negative sentiment scores
   * @param sentiments of text
   * @return separated sentiment scores
   */
  private def siftSentimentScores(sentiments: Seq[Double]): SiftSentiments = {
    val siftSentiments = SiftSentiments()

    sentiments.foldLeft(siftSentiments) (
      (siftSentiments,  sentiment) => {
        if (sentiment > 0)
          siftSentiments.posSum += (sentiment + 1) // compensates for neutral words that are counted as 1
        if (sentiment < 0)
          siftSentiments.negSum += (sentiment - 1)
        if (sentiment == 0)
          siftSentiments.neuCount += 1
        siftSentiments
      }
    )
  }

  private def roundWithDecimalPlaces(value: Double, decPlaces: Int): Double = {
    decPlaces match {
      case 3 => (value * 1000).round / 1000.toDouble
      case 4 => (value * 10000).round / 10000.toDouble
    }
  }

  /**
    * Replaces emojis in `input` with their textual descriptions.
    *
    * @param emojis emojis that are being checked for
    * @param input text containing `emojis`
    * @return text without `emojis` but with their textual descriptions
    */
  private def replaceEmojisWithDescription(emojis: List[String], input: String): String = {
    if (emojis.isEmpty) return input
    val emoji = emojis.head
    val textNoEmoji = input.replace(emoji, " ".concat(emojiLexikon.getOrElse(emoji, "")).concat(" "))
    //textNoEmoji = textNoEmoji.replace("  ", " ")
    if (emojis.tail.isEmpty) textNoEmoji
    else replaceEmojisWithDescription(emojis.tail, textNoEmoji)
  }

  /**
   * Creates sentiment lexicon dict
   *
   * @return sentiment lexicon
   */
  def makeLexDict(): Map[String, Double] = {
    val lexiconFile: Seq[String] = ResourceUtils.readFileAsListUTF(VADER_LEXICON_PATH)
    lexiconFile.map(line => {
        val lineArray = line.trim().split('\t')
        (lineArray(0) -> lineArray(1).toDouble)
      }
    ).toMap
  }

  /**
   * Creates the emoji dict
   *
   * @return emoji dictionary
   */
  def makeEmojiDict(): Map[String, String] = {
    val emojiLexiconFile: Seq[String] = ResourceUtils.readFileAsListUTF(VADER_EMOJI_LEXICON_PATH)
    emojiLexiconFile.map(line => {
        val lineArray = line.trim().split('\t')
        lineArray(0) -> lineArray(1)
      }
    ).toMap
  }
}
