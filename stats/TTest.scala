package stats

import common.rich.primitives.RichString._

object TTest extends Correlation {
	 override def apply(xs: Seq[Double], ys: Seq[Double]) =
		 new org.apache.commons.math3.stat.inference.TTest().tTest(xs.toArray, ys.toArray)
}
