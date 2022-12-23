package sentiment.utils

import scala.io.{ Source, Codec }
import java.io.PrintWriter

object ResourceUtils {

  def readResourceAsListUTF(path: String): Seq[String] = {
    // specify UTF8 for emoji lexikon
    Source.fromResource(path)(codec = Codec.UTF8).getLines().toList
  }

  // for evaluation purposes
  def readFileAsListUTF(path: String): Seq[String] = {
    // specify UTF8 for emoji lexikon
    Source.fromFile(path)(codec = Codec.UTF8).getLines().toList
  }

  // for evaluation purposes tweet_id -> (text, label, sentiment intensity, predicted_polarity)
  def writeMapToTSV(data: List[(String, String, Double, String)], filename: String): Unit = {
    val pw = new PrintWriter(filename)
    data.foreach {
      x => pw.write(s"${x._1}\t${x._2}\t${x._3}\t${x._4}\n")
    }
    pw.close()
  }
}
