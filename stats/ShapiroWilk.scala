package stats

import common.rich.primitives.RichString._

object ShapiroWilk {
	val r = RGetter.get
	def apply(xs: Seq[Double]) =
		r.capture(s"shapiro.test(${xs.mkString("c(", ",", ")")})")
			.split("\n")
			.last
			.captureWith( """.*?p-value [<=] (.*?)""".r)
			.toDouble
}
