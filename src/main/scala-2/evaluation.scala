import sentiment.utils.ResourceUtils

import org.apache.spark.SparkContext
import org.apache.spark.SparkConf
import org.apache.spark.mllib.evaluation.MulticlassMetrics

import java.io.PrintWriter
import java.time.LocalDateTime

/**
  * Main program for evaluations of  popular sentiment datasets.
  */
object EvaluationProgram {
  // Open a file for writing
  val currentDate: LocalDateTime = LocalDateTime.now()
  val predictOnSentenceLevel: Boolean = true
  val evalType = if (predictOnSentenceLevel) "sentence" else "text"
  val file = new java.io.File(s"additional_resources/results/summary/results_${evalType}_timestamp_${currentDate}.txt")
  val pw = new PrintWriter(file)

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf().setAppName("SparkSessionExample").setMaster("local[24]")
    val sc = new SparkContext(conf)
    sc.setLogLevel("ERROR") // less logging in console

    pw.println(s"timestamp: ${currentDate}")
    evaluate("Tweets ground truth", "tweets_GroundTruth.predicted.tsv", sc)
    // evaluate("Tweets ground truth", "tweets_GroundTruthLabels.predicted.tsv", sc)
    evaluate("Negation dataset", "negation_dataset.predicted.tsv", sc)
    evaluate("Sanction dataset", "sanktionen_evaluationsdatensatz.predicted.tsv", sc)
    evaluate("Wikipedia", "deu-wikipedia-2016-labeled.predicted.tsv", sc)
    evaluate("Emotions", "emotions.predicted.tsv", sc)
    evaluate("Filmstarts", "filmstarts.predicted.tsv", sc, noNeutrals=true)
    // evaluate("germeval2017", "germeval2017.predicted.tsv", sc)
    evaluate("Holidaycheck", "holidaycheck.china.predicted.tsv", sc, noNeutrals=true)
    evaluate("PotTS", "PotTS.predicted.tsv", sc)
    evaluate("Sb10k", "sb10k.predicted.tsv", sc)
    evaluate("Sb10k", "sb10k.predicted.tsv", sc, noNeutrals=true)
    // evaluate("Scare News Apps", "scare_news_apps.predicted.tsv", sc, noNeutrals=true)
    // evaluate("Scare Sport News", "scare_sport_news.predicted.tsv", sc, noNeutrals=true)
    // Close the file
    pw.close()
    sc.stop()
  }

  private def convertLabel(label: String): Double = {
    label match {
      case "pos" | "positive" | "positiv" => 1
      case "neg" | "negative" | "negativ" => -1
      case "neu" | "neutral" => 0
      case _ => {
        0
      }
    }
  }

  private def evaluate(dataName: String, file: String, sc: SparkContext, noNeutrals: Boolean = false): Unit = {
    pw.println("--------------------------------------")
    pw.println(dataName)
    if (noNeutrals) pw.println("----------------NO NEUTRALS----------------")
    val data = ResourceUtils.readFileAsListUTF(s"additional_resources/results/${evalType}/${file}")
    val predictionAndLabels =
      if (!noNeutrals) createPredictionAndLabels(data)
      else createPredictionAndLabels(data).filter( el => el._2 != 0 )
    val rdd = sc.parallelize(predictionAndLabels)
    val metrics = new MulticlassMetrics(rdd)
    pw.println(metrics.confusionMatrix)
    // Overall Statistics Accuracy == Micro-Average F1-Score
    val accuracy = metrics.accuracy
    pw.println("Summary Statistics")
    pw.println(s"Accuracy = $accuracy")

    pw.println("Summary Statistics")
    // Precision by label
    val labels = metrics.labels
    labels.foreach { l =>
      pw.println(s"Precision($l) = " + metrics.precision(l))
    }

    // Recall by label
    labels.foreach { l =>
      pw.println(s"Recall($l) = " + metrics.recall(l))
    }

    // False positive rate by label
    // labels.foreach { l =>
    //   pw.println(s"FPR($l) = " + metrics.falsePositiveRate(l))
    // }

    // F-measure by label
    val fMeasuresByLabel = labels.map { l => {
        val fMeasure =  metrics.fMeasure(l)
        pw.println(s"F1-Score($l) = " + fMeasure)
        fMeasure
      }
    }

    val macroAveragedF1 = fMeasuresByLabel.sum.toDouble/fMeasuresByLabel.size

    // macro-averaged F1-Score = arithmetic mean (aka unweighted mean) of all the per-class F1 scores
    pw.println(s"Macro-averaged F1-Score = ${macroAveragedF1}")
    pw.println(s"Weighted F1 score: ${metrics.weightedFMeasure}")

    // Weighted stats
    pw.println(s"Weighted precision: ${metrics.weightedPrecision}")
    pw.println(s"Weighted recall: ${metrics.weightedRecall}")
    // pw.println(s"Weighted false positive rate: ${metrics.weightedFalsePositiveRate}")
  }

  private def createPredictionAndLabels(data: Seq[String]): Seq[(Double, Double)] = {
    data.map(
      line => {
        val lineArray = line.trim().split('\t')
        try {
          val label = convertLabel(lineArray(1))
          val prediction = convertLabel(lineArray(3))
          (prediction, label)
        } catch {
          case e: IndexOutOfBoundsException => (0.0, 1.0)
          case e: Exception => (0.0, 1.0)
        }
      }
    )
  }
}
