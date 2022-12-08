package sentiment

import org.scalatest._

import flatspec._
import matchers._
import scala.collection.mutable.HashMap

import sentiment.SentiText

class SentiTextTest extends AnyFlatSpec with should.Matchers {

  "SentiText" should "wordsPlusPunc" in {
    val sentiText: SentiText = new SentiText("VADER")
    val result = sentiText.wordsPlusPunc()
    print(result)
    result shouldEqual HashMap("VADER??" -> "VADER", "VADER'" -> "VADER", ";VADER" -> "VADER", "VADER?!?" -> "VADER", "??VADER" -> "VADER", "!!!VADER" -> "VADER", "VADER." -> "VADER", "!VADER" -> "VADER", "VADER;" -> "VADER", "VADER?" -> "VADER", "!!VADER" -> "VADER", "VADER!" -> "VADER", "-VADER" -> "VADER", ",VADER" -> "VADER", "VADER!!" -> "VADER", "VADER," -> "VADER", "?!?VADER" -> "VADER", "VADER?!?!" -> "VADER", "?VADER" -> "VADER", "'VADER" -> "VADER", "VADER???" -> "VADER", "VADER" -> "VADER", ":VADER" -> "VADER", "!?!?VADER" -> "VADER", "VADER-" -> "VADER", "!?!VADER"-> "VADER", "VADER:" -> "VADER", "VADER!?!" -> "VADER", "VADER" -> "VADER", "?!?!VADER" -> "VADER", "VADER!!!" -> "VADER", "???VADER" -> "VADER", ".VADER" -> "VADER", "VADER!?!?" -> "VADER")
  }

  "SentiText" should "getWordsAndEmoticons" in {
  }
}
