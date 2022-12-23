

object EvaluationUtils {

  /**
    * Precision by label
    * precision of a class "positive" is the number of correctly predicted "positive" out of all predicted "positive"
    */
  def precision(predictionAndLabels: List[(String, String)], label: String): Double = {
    val predictedPositives = predictionAndLabels.filter(x => x._1 == label) // true positives + false positives
    val truePositive =  predictedPositives.count(x => { x._1 == x._2})
    truePositive.toDouble/predictedPositives.size
  }

  // Recall by label
  // the recall for "positive" is the number of correctly predicted "positive" out of the number of actual "positive"
  def recall(predictionAndLabels: List[(String, String)], label: String): Double = {
    val actualPositives = predictionAndLabels.filter(x => x._2 == label) // true positives + false positives
    val truePositive =  actualPositives.count(x => { x._1 == x._2})
    truePositive.toDouble/actualPositives.size
  }
}
