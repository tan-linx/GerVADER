package sentiment

import sentiment.SentimentIntensityAnalyzer
import sentiment.utils.{ResourceUtils, PreprocessingUtils, SentimentUtils}
import java.io.PrintWriter

import scala.io.StdIn.readLine

/**
  * Main program for calculate sentiment predictions.
  * 1. You can choose to predict the popular sentiment datasets.
  * 2. Hand over a .tsv file in (text label format), sentiments should be calculated for.
  * 3. Type a sentence sentiments should be calculated for.
  *
  * You can choose whether evaluation should be on sentence level (recommended) or on the entire text.
  */
object Program {
  // to predict on sentence level
  private var predictOnSentenceLevel: Boolean = false

  def main(args: Array[String]): Unit = {
    val sentenceLevel = readLine("Press [Enter] for sentence level evaluation of input (recommended): ")
    if(sentenceLevel.isEmpty) predictOnSentenceLevel = true

    val input = readLine("Press [Enter] if you want to predict wiki, emotions, germeval2019, holidaycheck, potTS and sb10k: ")
    if (input.isEmpty) {
      predictPublicSentimentDatasets()
    } else {
      val input = readLine("Predict type a) an entire file or b) a sentence: ")
      if (input == "a") {
        var fileName = readLine("Type name of file to predict (e.g. tweets.tsv format (text label)): ")
        // if fileN
        // val file = ResourceUtils.readFileAsListUTF("additional_resources/inputs/labeled_tweets.tsv")
        try {
          if (fileName.isEmpty) return
          val file = ResourceUtils.readFileAsListUTF(fileName)
          predict(file, textIndex=0, labelIndex=1, "predictions.tsv")
        } catch {
          case e: java.io.FileNotFoundException => println("Couldn't find that file.")
          case e: java.io.IOException => println("Had an IOException trying to read that file")
          case e: IndexOutOfBoundsException => println("Wrong file format. ")
          case e: Exception => println(e)
        }
      } else {
        var sentence = readLine("Type sentence to predict: ")
        val polarityScore = getSentimentIntensity(sentence)
        println(polarityScore)
      }
    }
  }

  private def predictPublicSentimentDatasets(): Unit = {
    val negations = ResourceUtils.readFileAsListUTF("additional_resources/inputs/negation_dataset.tsv")
    predict(negations, textIndex=0, labelIndex=1, "negation_dataset.predicted.tsv")
    val tweets = ResourceUtils.readFileAsListUTF("additional_resources/inputs/sanktionen_evaluationsdatensatz.tsv")
    predict(tweets, textIndex=1, labelIndex=2, "sanktionen_evaluationsdatensatz.predicted.tsv")
    val wiki = ResourceUtils.readFileAsListUTF("additional_resources/inputs/deu-wikipedia-2016-labeled.sampled.tsv")
    predict(wiki, 1, 0, "deu-wikipedia-2016-labeled.predicted.tsv")
    val emotions = ResourceUtils.readFileAsListUTF("additional_resources/inputs/emotions_german_sentibert.tsv")
    predict(emotions, 1, 0, "emotions.predicted.tsv")
    val germeval2017 = ResourceUtils.readFileAsListUTF("additional_resources/inputs/germeval2017.tsv")
    predict(germeval2017, textIndex=1, labelIndex=3, "germeval2017.predicted.tsv")
    val holidaycheck = ResourceUtils.readFileAsListUTF("additional_resources/inputs/holidaycheck.china.sampled.edited.tsv")
    predict(holidaycheck, textIndex=2, labelIndex=1, "holidaycheck.china.predicted.tsv")
    val potTS = ResourceUtils.readFileAsListUTF("additional_resources/inputs/PotTS.tsv")
    predict(potTS, textIndex=1, labelIndex=0, "PotTS.predicted.tsv")
    val sb10k = ResourceUtils.readFileAsListUTF("additional_resources/inputs/sb10k_corpus_v1.0.cgsa.tsv")
    predict(sb10k, textIndex=2, labelIndex=1, "sb10k.predicted.tsv")

    // val scareNewsAppsSubj = ResourceUtils.readFileAsListUTF("additional_resources/inputs/scare_news_apps_subjective_phrases.tsv")
    // predict(scareNewsAppsSubj, textIndex=4, labelIndex=6, "scare_news_apps_subjective_phrases.predicted.tsv")

    // val scareSportNewsSubj = ResourceUtils.readFileAsListUTF("additional_resources/inputs/scare_sport_news_subjective_phrases.tsv")
    // predict(scareSportNewsSubj, textIndex=4, labelIndex=6, "scare_sport_news_subjective_phrases.predicted.tsv")
    val scareNewsApps = ResourceUtils.readFileAsListUTF("additional_resources/inputs/scare_news_apps.edited.tsv")
    predict(scareNewsApps, textIndex=3, labelIndex=1, "scare_news_apps.predicted.tsv")
    val scareSportNews = ResourceUtils.readFileAsListUTF("additional_resources/inputs/scare_sport_news.edited.tsv")
    predict(scareSportNews, textIndex=3, labelIndex=1, "scare_sport_news.predicted.tsv")
    val filmstarts = ResourceUtils.readFileAsListUTF("additional_resources/inputs/filmstarts.sampled.tsv")
    predict(filmstarts, textIndex=2, labelIndex=1, "filmstarts.predicted.tsv")
  }

  /**
    * Map each line in data to (text, label, polarityScore, polarity) & write map to additional_resources/results/`outputFile`
    * @param textIndex index of column containing text to anayze
    * @param labelIndex index of column containing the sentiment label
    * @param outputFile name of output file
    */
  private def predict(data: Seq[String], textIndex: Int, labelIndex: Int, outputFile: String): Unit = {
    val predictions = data.map(
      line => {
        val lineArray = line.trim().split('\t')
        val label = PreprocessingUtils.cleanLabel(lineArray(labelIndex))
        val text = lineArray(textIndex)
        val sentimentIntensity: Double = getSentimentIntensity(text)
          //analyzer.polarityScores(text).compound  // better: average polarity scores of each sentence
        val polarity = SentimentUtils.getPolarity(sentimentIntensity)
        (text, label, sentimentIntensity, polarity)
      }
    ).filter(line => line._2 != "unknown").toList
    var folder = if (predictOnSentenceLevel) "sentence" else "text"
    ResourceUtils.writeMapToTSV(predictions, s"additional_resources/results/${folder}/${outputFile}")
  }

  private def getSentimentIntensity(text: String): Double = {
    val analyzer = new SentimentIntensityAnalyzer
    if (predictOnSentenceLevel) {
      analyzer.polarityScoresSentenceLevel(text)
    } else {
      analyzer.polarityScores(text).compound
    }
  }
}
