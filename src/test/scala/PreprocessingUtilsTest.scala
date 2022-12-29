

import org.scalatest._
import flatspec._
import matchers._

import sentiment.utils.PreprocessingUtils
import scala.runtime.Statics

class PreprocessingUtilsTest extends AnyFlatSpec with should.Matchers {

  "SentimentUtils" should "splitSentencesML" in {
    val sentences = PreprocessingUtils.splitSentencesML("@XQ3CTY @Sabse1962 @alpenmaster @philippvetter @bnetza @welt Hat Putin uns, die Nato oder die EU angegriffen? Haben wir Sanktionen verhängt als die USA völkerrechtswidrig Libyen, Serbien, Syrien und den Irak etc. angegriffen haben? Sie können Ihre Doppelmoral behalten.")
    sentences(1) shouldEqual "Haben wir Sanktionen verhängt als die USA völkerrechtswidrig Libyen, Serbien, Syrien und den Irak etc. angegriffen haben?"

    val sentences1 = PreprocessingUtils.splitSentencesML("Oh ... Wir haben einen neuen Papst ??? Jetzt kehrt hier endlich wieder Ruhe ein .")
    sentences1(1) shouldEqual "Wir haben einen neuen Papst ??? Jetzt kehrt hier endlich wieder Ruhe ein ."

    val sentences2 = PreprocessingUtils.splitSentencesML("Weiter wegschauen? Wie schon nach der Annexion der Krim?")
    sentences2(1) shouldEqual "Wie schon nach der Annexion der Krim?"

    // does not split sentences at multiple punctuations
    val sentences3 = PreprocessingUtils.splitSentencesML("Waffen liefern, Sanktionen. Habt ihr auch was von Cyber Krieg gehört ?!! Kriege muss man nicht mehr mit Pferd und Schwert ausführen!!")
    sentences3(1) shouldEqual "Habt ihr auch was von Cyber Krieg gehört ?!! Kriege muss man nicht mehr mit Pferd und Schwert ausführen!!"
  }

  "SentimentUtils" should "splitSentencesRegex" in {
    val sentences = PreprocessingUtils.splitSentencesRegex("@XQ3CTY @Sabse1962 @alpenmaster @philippvetter @bnetza @welt Hat Putin uns, die Nato oder die EU angegriffen? Haben wir Sanktionen verhängt als die USA völkerrechtswidrig Libyen, Serbien, Syrien und den Irak etc. angegriffen haben? Sie können Ihre Doppelmoral behalten.")

    // splits sentences at abbreviations..
    sentences(1) shouldEqual "Haben wir Sanktionen verhängt als die USA völkerrechtswidrig Libyen, Serbien, Syrien und den Irak etc."

    val sentences1 = PreprocessingUtils.splitSentencesRegex("Oh ... Wir haben einen neuen Papst ??? Jetzt kehrt hier endlich wieder Ruhe ein .")
    sentences1(1) shouldEqual "Wir haben einen neuen Papst ???"
    val sentences2 = PreprocessingUtils.splitSentencesRegex("Weiter wegschauen? Wie schon nach der Annexion der Krim?")
    sentences2(1) shouldEqual "Wie schon nach der Annexion der Krim?"

    val sentences3 = PreprocessingUtils.splitSentencesRegex("Waffen liefern, Sanktionen. Habt ihr auch was von Cyber Krieg gehört ?!! Kriege muss man nicht mehr mit Pferd und Schwert ausführen!!")
    sentences3(1) shouldEqual "Habt ihr auch was von Cyber Krieg gehört ?!!"
  }

  "SentimentUtils" should "splitSentences" in {
    val sentences = PreprocessingUtils.splitSentences("@XQ3CTY @Sabse1962 @alpenmaster @philippvetter @bnetza @welt Hat Putin uns, die Nato oder die EU angegriffen? Haben wir Sanktionen verhängt als die USA völkerrechtswidrig Libyen, Serbien, Syrien und den Irak etc. angegriffen haben? Sie können Ihre Doppelmoral behalten.")
    sentences(1) shouldEqual "Haben wir Sanktionen verhängt als die USA völkerrechtswidrig Libyen, Serbien, Syrien und den Irak etc. angegriffen haben?"

    val sentences1 = PreprocessingUtils.splitSentences("Oh ... Wir haben einen neuen Papst ??? Jetzt kehrt hier endlich wieder Ruhe ein .")
    sentences1(1) shouldEqual "Wir haben einen neuen Papst ???"

    val sentences2 = PreprocessingUtils.splitSentences("Weiter wegschauen? Wie schon nach der Annexion der Krim?")
    sentences2(1) shouldEqual "Wie schon nach der Annexion der Krim?"

    // does not split sentences at multiple punctuations
    val sentences3 = PreprocessingUtils.splitSentences("Waffen liefern, Sanktionen. Habt ihr auch was von Cyber Krieg gehört ?!! Kriege muss man nicht mehr mit Pferd und Schwert ausführen!!")
    sentences3(1) shouldEqual "Habt ihr auch was von Cyber Krieg gehört ?!!"
  }

  "SentimentUtils" should "cleanLabels" in {
    PreprocessingUtils.cleanLabel("pos") shouldEqual "positive"
    PreprocessingUtils.cleanLabel("neg") shouldEqual "negative"
    PreprocessingUtils.cleanLabel("neu") shouldEqual "neutral"
    PreprocessingUtils.cleanLabel("unknown") shouldEqual "unknown"
    PreprocessingUtils.cleanLabel("positive") shouldEqual "positive"
  }
}
