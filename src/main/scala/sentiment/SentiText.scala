package sentiment

import sentiment.utils.SentimentUtils
import util.control.Breaks._

/**
 * Identify sentiment-relevant string-level properties of input `text`.
 * @param text
 */
private[sentiment] class SentiText(val text: String) {
  val wordsAndEmoticons: Seq[String] = getWordsAndEmoticons()
  val isCapDifferential: Boolean = SentimentUtils.allCapDifferential(wordsAndEmoticons)

  /**
   * Removes all trailing and leading punctuation.
   * If the resulting string has two or fewer characters,
   * then it was likely an emoticon, so return original string
   * (ie ":)" stripped would be "", so just return ":)".
   *
   * @param token token including punctuations
   * @param puncList list of punctuations to remove
   * @return `token` without puncutations
   */
  private def stripPuncIfWord(token: String, puncList: List[String]): String= {
    val punc = puncList.head
    val stripped = token.stripPrefix(punc).stripSuffix(punc)
    if (stripped.size <= 2) token
    else if (puncList.tail == List.empty) stripped
    else stripPuncIfWord(stripped, puncList.tail)
  }

  /**
   * Tokenizes `text`.
   * Removes leading and trailing punctuation. Leaves contractions and most emoticons.
   *
   * @return tokens of `text`
   */
  private def getWordsAndEmoticons(): List[String] = {
    var wes: List[String] = text.split(" ").toList
    wes.map(x => stripPuncIfWord(x, SentimentUtils.puncList)).filter(x => x.length > 0).toList
  }
}
