
package mains.programs

import Jama.Matrix
import corpus._
import corpus.bugs.CorporaDataWithBugs
import corpus.rich.RichData._
import mains.generators.lists.MetricsDescriptor
import mains.generators.plots.MeanBugsToSize
import mains.generators.tables._
import mains.generators.tables.bugs._
import mains.generators.tables.dataset.DatasetFeatures
import mains.generators.tables.pca.{PCAPerCorpusCosine, PCAAngles, PCAEigenValues}
import mains.generators.transformators._
import mains.programs.pca.RemoveSizeFromAngle
import metrics._
import stats._

/**
 * Same flavour as lord of the main, zero the calories!
 * Does not run "heavy computations" (e.g., parsing metrics)
 */
object LordOfTheMainsPca extends Debug {

	override def timedMain {
		val data = MetricValuesParser.getData.withoutTests
		RemoveSizeFromAngle(data)
		PCAPerCorpusCosine(data)
	}
}
