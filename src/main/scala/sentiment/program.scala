import sentiment.SentimentIntensityAnalyzer

object Program {
  def main(args: Array[String]): Unit = {

    val analyzer = new SentimentIntensityAnalyzer

    val text = "The party was good but appetizers and drinks were poorly selected."

    val results = analyzer.polarityScores(text)

    println(s"Positive score: $results.positive")
    println(s"Negative score: $results.negative")
    println(s"Neutral score:  $results.neutral")
    println(s"Compound score: $results.compound")
  }
}