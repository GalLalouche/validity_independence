package alg

import scala.collection.mutable

object LempelZivForStrings {
  def apply(strings: TraversableOnce[String]) = compress(strings)
  def compress(strings: TraversableOnce[String]): Map[String, Int] = {
    val dict: mutable.Map[String, Int] = mutable.Map() ++ strings.toSet.zipWithIndex.toMap
    var w: String = ""
    for (k ← strings) {
      if (dict.contains(w + k))
        w = w + k
      else {
        dict(w + k) = dict.size
        w = k
      }
    }
    dict.toMap
  }
}