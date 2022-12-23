package sentiment

import sentiment.SentimentIntensityAnalyzer
import sentiment.utils.{ResourceUtils, PreprocessingUtils}
import java.io.PrintWriter

object Program {
  val sentenceLevelEvaluation: Boolean = false
  val folder = if (sentenceLevelEvaluation) "sentence" else "text"

  def main(args: Array[String]): Unit = {
    val tweets = ResourceUtils.readFileAsListUTF("additional_resources/inputs/labeled_tweets.tsv")
    createEvaluationMap(tweets, textIndex=1, labelIndex=2, "labeled_tweets.predicted.tsv")

    val wiki = ResourceUtils.readFileAsListUTF("additional_resources/inputs/deu-wikipedia-2016-labeled.sampled.tsv")
    createEvaluationMap(wiki, 1, 0, "deu-wikipedia-2016-labeled.predicted.tsv")

    val emotions = ResourceUtils.readFileAsListUTF("additional_resources/inputs/emotions_german_sentibert.tsv")
    createEvaluationMap(emotions, 1, 0, "emotions.predicted.tsv")

    val germeval2017 = ResourceUtils.readFileAsListUTF("additional_resources/inputs/germeval2017.tsv")
    createEvaluationMap(germeval2017, textIndex=1, labelIndex=3, "germeval2017.predicted.tsv")

    val holidaycheck = ResourceUtils.readFileAsListUTF("additional_resources/inputs/holidaycheck.china.sampled.edited.tsv")
    createEvaluationMap(holidaycheck, textIndex=2, labelIndex=1, "holidaycheck.china.predicted.tsv")

    val potTS = ResourceUtils.readFileAsListUTF("additional_resources/inputs/PotTS.tsv")
    createEvaluationMap(potTS, textIndex=1, labelIndex=0, "PotTS.predicted.tsv")

    val sb10k = ResourceUtils.readFileAsListUTF("additional_resources/inputs/sb10k_corpus_v1.0.cgsa.tsv")
    createEvaluationMap(sb10k, textIndex=2, labelIndex=1, "sb10k.predicted.tsv")

    // val scareNewsAppsSubj = ResourceUtils.readFileAsListUTF("additional_resources/inputs/scare_news_apps_subjective_phrases.tsv")
    // createEvaluationMap(scareNewsAppsSubj, textIndex=4, labelIndex=6, "scare_news_apps_subjective_phrases.predicted.tsv")

    // val scareSportNewsSubj = ResourceUtils.readFileAsListUTF("additional_resources/inputs/scare_sport_news_subjective_phrases.tsv")
    // createEvaluationMap(scareSportNewsSubj, textIndex=4, labelIndex=6, "scare_sport_news_subjective_phrases.predicted.tsv")

    val scareNewsApps = ResourceUtils.readFileAsListUTF("additional_resources/inputs/scare_news_apps.edited.tsv")
    createEvaluationMap(scareNewsApps, textIndex=3, labelIndex=1, "scare_news_apps.predicted.tsv")

    val scareSportNews = ResourceUtils.readFileAsListUTF("additional_resources/inputs/scare_sport_news.edited.tsv")
    createEvaluationMap(scareSportNews, textIndex=3, labelIndex=1, "scare_sport_news.predicted.tsv")

    val filmstarts = ResourceUtils.readFileAsListUTF("additional_resources/inputs/filmstarts.sampled.tsv")
    createEvaluationMap(filmstarts, textIndex=2, labelIndex=1, "filmstarts.predicted.tsv")
  }

  private def createEvaluationMap(data: Seq[String], textIndex: Int, labelIndex: Int, outputFile: String): Unit = {
    val predictions = data.map(
      line => {
        val lineArray = line.trim().split('\t')
        val label = cleanLabel(lineArray(labelIndex))
        val text = lineArray(textIndex)
        val sentimentIntensity: Double = getSentimentIntensity(text)
          //analyzer.polarityScores(text).compound  // better: average polarity scores of each sentence
        val polarity = getPolarity(sentimentIntensity)
        (text, label, sentimentIntensity, polarity)
      }
    ).filter(line => line._2 != "unknown").toList
    ResourceUtils.writeMapToTSV(predictions, s"additional_resources/results/${folder}/${outputFile}")
  }

  private def getPolarity(sentimentIntensity: Double): String = {
    if (sentimentIntensity > 0) "positive"
    else if (sentimentIntensity < 0) "negative"
    else "neutral"
  }

  private def cleanLabel(label: String): String = {
    label match {
      case "pos" | "Positive" => "positive"
      case "neg" | "Negative"=> "negative"
      case "neu" | "Neutral" => "neutral"
      case  "unknown" | "Unknown" | "mixed" => "unknown"
      case _ => label
    }
  }

  private def getSentimentIntensity(text: String): Double = {
    val analyzer = new SentimentIntensityAnalyzer
    if (sentenceLevelEvaluation) {
      val textSplitted = PreprocessingUtils.splitSentences(text)
      val sumScores: Double = textSplitted.map(sentence => analyzer.polarityScores(sentence).compound).sum
      return sumScores.toDouble/textSplitted.size
    }
    analyzer.polarityScores(text).compound
  }
}
