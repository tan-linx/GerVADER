package sentiment

import scala.annotation.tailrec

/**
 * Identify sentiment-relevant string-level properties of input `text`.
 * @param text
 */
private[sentiment] class SentiText(val text: String) {
  val wordsAndEmoticons: Seq[String] = getWordsAndEmoticons()

  /**
   * Tokenizes `text`.
   * Removes leading and trailing punctuation. Leaves contractions and most emoticons.
   *
   * @return tokens of `text`
   */
  private def getWordsAndEmoticons(): List[String] = {
    val wes: List[String] = text.split(" ").toList
    wes.map(x => SentiText.stripPuncIfWord(x, SentiText.puncList)).filter(x => x.length > 0).toList
  }
}

private[sentiment] object SentiText {
  private val puncList = List(
    ",,",".", "!", "?", ",", ";", ":", "-", "'", "\"", "!!", "!!!",
    "??", "???", "?!?", "!?!", "?!?!", "!?!?", "“","„", "““", "\"", "\"\"",
    "..", "...", "....", "#", "*", "«", "««", "»", "»»", "\"!", "!\"", ".\""
  )

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
  @tailrec
  private def stripPuncIfWord(token: String, puncList: List[String]): String = {
    puncList match {
      case punctuation :: next => {
        val stripped = token.stripPrefix(punctuation).stripSuffix(punctuation)
        if (stripped.size <= 2) token
        else if (next.isEmpty) stripped
        else SentiText.stripPuncIfWord(stripped, next)
      }
      case Nil => token
    }
  }
}
