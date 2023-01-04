package sentiment.utils

private[sentiment] object SentimentUtils {

  val BIncr: Double = 0.293
  val BDecr: Double = -0.293
  // Cap Increase
  val CIncr: Double = 0.733
  val NScalar: Double = -0.74

  val puncList = List(
    ".", "!", "?", ",", ";", ":", "-", "'", "\"", "!!", "!!!",
    "??", "???", "?!?", "!?!", "?!?!", "!?!?"
  )

  val negate = List(
    "nicht", "nie",
    "never", "niemals",
    "kein", "keine", "keinen", "keinem", "nope", "nichts", "weder", "nix", "nirgendwo",
    "uhuh","uh-uh",
    "ohne", "selten", "kaum", "seltenst", "rar", "trotz", "trotzdem", "obwohl"
  )

  val boosterDict = Map[String, Double](
    "absolut" -> BIncr, "total" -> BIncr, "unbedingt" -> BIncr, //"absolutely" -> BIncr,
    "erstaunlich" -> BIncr,  //"amazingly" -> BIncr,
    "enorm" -> BIncr, //"awfully" -> BIncr,
    "komplett" -> BIncr, "restlos" -> BIncr, "gänzlich" -> BIncr, "völlig" -> BIncr,    //"completely" -> BIncr,
    "beträchtlich" -> BIncr, "erheblich" -> BIncr, "deutlich" -> BIncr, //"considerably" -> BIncr,
    "ausgesprochen" -> BIncr, "eindeutig" -> BIncr, //"decidedly" -> BIncr,
    "zutiefst" -> BIncr, //"deeply" -> BIncr,
    "verdammt" -> BIncr, //"effing" -> BIncr,
    "überaus" -> BIncr, //"enormously" -> BIncr,
    //"entirely" -> BIncr,
    "besonders" -> BIncr, // "especially" -> BIncr,
    "außerordentlich" -> BIncr, "außergewöhnlich" -> BIncr, "ungemein" -> BIncr,  //"exceptionally" -> BIncr,
    "äußerst" -> BIncr, "extrem" -> BIncr, //"extremely" -> BIncr,
    "sagenhaft" -> BIncr, //"fabulously" -> BIncr,
    //"flipping" -> BIncr,
    //"flippin" -> BIncr,
    //"fricking" -> BIncr,
    //"frickin" -> BIncr,
    //"frigging" -> BIncr,
    //"friggin" -> BIncr,
    //"fully" -> BIncr,
    "verfickt" -> BIncr, //"fucking" -> BIncr,
    "sehr" -> BIncr, "massiv" -> BIncr, "mega" -> BIncr,   //"greatly" -> BIncr,
    //"hella" -> BIncr,
    "höchst" -> BIncr, //"highly" -> BIncr,
    "immens" -> BIncr, //"hugely" -> BIncr,
    //"incredibly" -> BIncr,
    //"intensely" -> BIncr,
    //"majorly" -> BIncr,
    //"more" -> BIncr,
    //"most" -> BIncr,
    //"particularly" -> BIncr,
    //"purely" -> BIncr,
    //"quite" -> BIncr,
    //"really" -> BIncr,
    //"remarkably" -> BIncr,
    "so" -> BIncr,
    "wesentlich" -> BIncr,  //"substantially" -> BIncr,
    //"thoroughly" -> BIncr,
    //"totally" -> BIncr,
    //"tremendously" -> BIncr,
    "über" -> BIncr, //"uber" -> BIncr,
    //"unbelievably" -> BIncr,
    //"unusually" -> BIncr,
    //"utterly" -> BIncr,
    //"very" -> BIncr,
    //////////
    "fast" -> BDecr, "nahezu" -> BDecr, "beinahe" -> BDecr, "beinah" -> BDecr, //"almost" -> BDecr,
    //"barely" -> BDecr,
    "kaum" -> BDecr, //"hardly" -> BDecr,
    "geradeso" -> BDecr, //"just enough" -> BDecr,
    "irgendwie" -> BDecr, "ziemlich" -> BDecr, "quasi" -> BDecr, "ein bisschen" -> BDecr,
  //"kind of" -> BDecr,
    //"kinda" -> BDecr,
    //"kindof" -> BDecr,
    //"kind-of" -> BDecr,
    //"less" -> BDecr,
    //"little" -> BDecr,
    "geringfügig" -> BDecr, "unwesentlich" -> BDecr, "marginal" -> BDecr, //"marginally" -> BDecr,
    "zeitweise" -> BDecr, "gelegentlich" -> BDecr, "mitunter" -> BDecr, //"occasionally" -> BDecr,
    "halbwegs" -> BDecr, "teils" -> BDecr, "teilweise" -> BDecr, //"partly" -> BDecr,
    //"scarcely" -> BDecr,
    "etwas" -> BDecr, "bisschen" -> BDecr, //"slightly" -> BDecr,
    "einigermaßen" -> BDecr, //"somewhat" -> BDecr,
    "derlei" -> BDecr, "gewissermaßen" -> BDecr, //"sort of" -> BDecr,
    //"sorta" -> BDecr,
    //"sortof" -> BDecr,
    //"sort-of" -> BDecr
  )

  val specialCaseIdioms = Map[String, Double](
    "der scheiß" -> 3,
    "the shit" -> 3,
    "the bomb" -> 3,
    "bad ass" -> 1.5,
    "yeah right" -> -2,
    "cut the mustard" -> 2,
    "kiss of death" -> -1.5,
    "hand to mouth" -> -2
  )

  /**
   * Determine if `inputWords` contains negation words
   *
   * @param inputWords
   * @param includenT `True` if n't should be analyzed
   * @return `True` if `inputWords` contains negation
   */
  def negated(inputWords: List[String], includenT: Boolean = true): Boolean = {
    val inputWordsLC = inputWords.map(_.toLowerCase())

    // Helper to determine if input contains negation words
    def containsNegation(inputWords: List[String], negations: List[String]): Boolean = {
      if (inputWords.contains(negations.head)) true
      else if (negations.tail == List.empty) false
      else containsNegation(inputWords, negations.tail)
    }
    if (containsNegation(inputWordsLC, negate)) return true


    // Helper to determine if input contains nt
    def containsnT(inputWords: List[String]): Boolean = {
      if (inputWords.head.contains("n't")) true
      else if (inputWords.tail == List.empty) false
      else containsnT(inputWords.tail)
    }
    if (includenT && containsnT(inputWordsLC)) return true

    /* if (inputWords.contains("least")) {
      val i = inputWords.indexOf("least")
      if (i > 0 && inputWords(i - 1) != "at") {
        return true
      }
    } */
    false
  }


  /**
   * Normalizes score to be between -1 and 1
   *
   * @param score
   * @param alpha
   * @return normalized score
   */
  def normalize(score: Double, alpha: Double = 15): Double = {
    val normScore: Double = score / math.sqrt(score * score + alpha)

    if (normScore < -1.0)
      -1.0
    else if (normScore > 1.0)
      1.0
    else
      normScore
  }

  /**
   * Checks whether some but not all words in `words` are ALL-CAPS
   *
   * @param words
   * @return `True` if some but not all items in `words` are ALL-CAPS
   */
  def allCapDifferential(words: Seq[String]): Boolean = {
   val allCapWords = words.foldLeft(0)(
      (allCapWords, word) => {
        if (isUpper(word)) allCapWords + 1
        else allCapWords
      }
    )
    val capDifferential = words.size - allCapWords
    capDifferential > 0 && capDifferential < words.size
  }

  /**
   * Increases/decreases scalar if `word` is a booster
   *
   * @param word potential booster word
   * @param valence valence of word following booster word
   * @param isCapDiff `True` if there is a cap differential in the sentence
   * @return
   */
  def scalarIncDec(word: String, valence: Double, isCapDiff: Boolean): Double = {
    val wordLower: String = word.toLowerCase()
    if (!boosterDict.contains(wordLower)) {
      0.0
    } else {
      var scalar: Double = boosterDict(wordLower)
      if (valence < 0) {
        scalar *= -1
      }
      if (isUpper(word) && isCapDiff) {
        if (valence > 0) {
          scalar += CIncr
        } else {
          scalar += -CIncr
        }
      }
      scalar
    }
  }

  /**
   * Checks if `cs` is in ALL-CAPs
   *
   * @param cs
   * @return `True` if `cs` is in ALLCAPs
   */
  def isUpper(cs: String): Boolean = {
    cs.forall(c => Character.isUpperCase(c))
  }

  /**
   * Return polarity for given sentimentIntensity
   *
   */
  def getPolarity(sentimentIntensity: Double): String = {
      if (sentimentIntensity > 0) "positive"
      else if (sentimentIntensity < 0) "negative"
      else "neutral"
  }
}
