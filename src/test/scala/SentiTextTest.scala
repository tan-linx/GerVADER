package sentiment

import org.scalatest._

import flatspec._
import matchers._
import scala.collection.mutable.HashMap

import sentiment.SentiText

class SentiTextTest extends AnyFlatSpec with should.Matchers {

  "SentiText" should "getWordsAndEmoticons (Tokenization)" in {
    val sentiTextWithKindOf = new SentiText("The book was kind of good")
    sentiTextWithKindOf.wordsAndEmoticons shouldEqual Seq("The", "book", "was", "kind", "of", "good")

    val puncAndEmojiText = new SentiText(".Removes :leading: and ,trailing !puncutation!!! :) :-)")
    puncAndEmojiText.wordsAndEmoticons shouldEqual Seq("Removes", "leading", "and", "trailing", "puncutation", ":)", ":-)")

    val emojiAndPuncText = new SentiText("Removes leading and trailing puncutation :)!")
    emojiAndPuncText.wordsAndEmoticons shouldEqual Seq("Removes", "leading", "and", "trailing", "puncutation", ":)!")

    val emojiText = new SentiText("Leaves most emoticons >:-)")
    emojiText.wordsAndEmoticons shouldEqual Seq("Leaves", "most", "emoticons", ">:-)")

    val contractionText = new SentiText("VADER-Algorithms leaves contractions")
    contractionText.wordsAndEmoticons shouldEqual Seq("VADER-Algorithms", "leaves", "contractions")

    // "||-:"" does not get recognized
    val pureEmojiText = new SentiText("{:  >:-) |-0 |-: |-o |= |^: |o: |;-)")
    pureEmojiText.wordsAndEmoticons shouldEqual Seq("{:", ">:-)", "|-0","|-:", "|-o", "|=", "|^:", "|o:", "|;-)")

    val puncPlusLetterEmoji = new SentiText("Does not preserve punc-plus-letter emoticons :D")
    puncPlusLetterEmoji.wordsAndEmoticons shouldEqual Seq("Does", "not", "preserve", "punc-plus-letter", "emoticons", ":D")

    val abbreviations = new SentiText("ca. 100.000 Jahre 1.Klasse 1. Klasse")
    abbreviations.wordsAndEmoticons shouldEqual Seq("ca.", "100.000", "Jahre", "1.Klasse", "1.", "Klasse")

    val sentiText = new SentiText("The book was kind of good.")
    sentiText.wordsAndEmoticons shouldEqual Seq("The", "book", "was", "kind", "of", "good")

    // this sentence fails on @zirayal project due to tokenization (removal of punctuation)
    val complexSentiText = new SentiText("The plot was good, but the characters are uncompelling and the dialog is not great.")
    complexSentiText.wordsAndEmoticons shouldEqual Seq("The", "plot", "was", "good", "but", "the", "characters", "are", "uncompelling", "and", "the", "dialog", "is", "not", "great")

    // should tokenze "a", only relevant for english analysis
    val sentiTextWithA = new SentiText("it isn't a horrible book.")
    sentiTextWithA.wordsAndEmoticons shouldEqual Seq("it", "isn't", "a", "horrible", "book")
  }
}
