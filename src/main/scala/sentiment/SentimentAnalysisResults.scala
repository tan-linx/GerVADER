package sentiment


/**
  * The 'positive', 'neutral', and 'negative' scores should all add up to be 1 (Useful for multidimensional measures of sentiment).
  * The 'compound' score is computed by summing the valence scores of each word in the lexicon, adjusted according to the rules, and then normalized to be between -1 (most extreme negative) and +1 (most extreme positive) (Hutto & Gilbert)
  * @param negative ratio of negative sentiments in text
  * @param neutral ratio of positive sentiments in text
  * @param positive ratio of neutral sentiments in text
  * @param compound normalized, weighted composite score
  */
case class SentimentAnalysisResults(
  negative: Double = 0,
  neutral: Double = 0,
  positive: Double = 0,
  compound: Double = 0
)
