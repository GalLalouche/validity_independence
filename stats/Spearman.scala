package stats

object Spearman extends CorrelationWithPValue {

  def withPValue(x: Seq[Double], y: Seq[Double]): (Double, Double) = {
    val $ = new SpearmansCorrelation().correlation(x toArray, y toArray)
    ($.getFirst, $.getSecond)
  }
}