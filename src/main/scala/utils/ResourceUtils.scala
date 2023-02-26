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
}
