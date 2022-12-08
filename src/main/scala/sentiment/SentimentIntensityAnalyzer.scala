
package sentiment

import scala.collection.mutable.ListBuffer
import scala.collection.{Seq, _}

/**
 * An abstraction to represent the sentiment intensity analyzer.
 */
class SentimentIntensityAnalyzer {

  /**
   * Return metrics for positive, negative and neutral sentiment based on the input text.
   *
   * @param input
   * @return
   */
  def polarityScores(input: String): SentimentAnalysisResults = {
    var sentiments: ListBuffer[Double] = ListBuffer[Double]()
    scoreValence(sentiments, input)
  }

  def scoreValence(sentiments: Seq[Double], text: String): SentimentAnalysisResults = {
    SentimentAnalysisResults(
      compound = 0,
      positive = 0,
      negative = 0,
      neutral = 0
    )
  }
}