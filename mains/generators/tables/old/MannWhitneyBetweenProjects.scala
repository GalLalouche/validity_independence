package mains.generators.tables.old

import stats.MannWhitneyCorrelation

object MannWhitneyBetweenProjects extends MetricConsistencyBetweenProjects {
  override val correlation = MannWhitneyCorrelation
    override val baseSignificance = 0.05
}
