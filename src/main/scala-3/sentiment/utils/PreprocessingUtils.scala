package sentiment.utils

import opennlp.tools.sentdetect.SentenceDetectorME
import opennlp.tools.sentdetect.SentenceModel
import scala.util.matching.Regex

object PreprocessingUtils {
    private val modelIn = new java.io.FileInputStream("./additional_resources/models/de-sent.bin")
    private val model = new SentenceModel(modelIn)
    private val sentenceDetector = new SentenceDetectorME(model)

    def splitSentencesML(text: String): Array[String]= {
        sentenceDetector.sentDetect(text)
    }

    def splitSentencesRegex(text: String): Array[String]= {
        val pattern: Regex = "(?<=[.!?])\\s".r
        pattern.split(text)
    }

    /**
      * VADER works best when analysis is done at the sentence level (but it can work on single words or entire novels).
      *
      * @param text
      * @return
      */
    def splitSentences(text: String): Array[String]= {
        val sentences = sentenceDetector.sentDetect(text)
        val pattern: Regex = "(?<=[!?])\\s".r
        sentences.flatMap(x => pattern.split(x))
    }

    def cleanLabel(label: String): String = {
        label match {
            case "pos" | "Positive" => "positive"
            case "neg" | "Negative"=> "negative"
            case "neu" | "Neutral" => "neutral"
            case  "unknown" | "Unknown" | "mixed" => "unknown"
            case _ => label
        }
    }
}
