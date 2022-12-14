package sentiment.utils

import scala.io.{ Source, Codec }

private[sentiment] object ResourceUtils {

  def readFileAsListUTF(path: String): Seq[String] = {
    // specify UTF8 for emoji lexikon
    Source.fromResource(path)(codec = Codec.UTF8).getLines().toList
  }
}
