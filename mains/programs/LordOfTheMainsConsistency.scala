
package mains.programs

import corpus.rich.RichData._
import mains.generators.tables.old.MannWhitneyBetweenProjects

/**
 * Only generates data necessary for consistency
 */
object LordOfTheMainsConsistency extends Debug {

	override def timedMain {
		timed("EntireRun") {
			val raw = MetricValuesParser.getData.ignoreMetrics(Configuration.ignoredMetrics).getAverageValuesPerFile
			//      MetricsDescriptor.apply(rawData.metrics)
			//      MetricsCCDF.apply(rawData.metrics)
			MannWhitneyBetweenProjects(raw, Configuration.namedNormalizations)
			MannWhitneyBetweenProjects.withTableName(_ + "Median")(raw, Configuration.medianNormalizations)
		}
	}
}
