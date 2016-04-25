package metrics

import org.eclipse.jdt.core.dom._

object LoopCounter extends SetCountingMetric(Set(classOf[WhileStatement], classOf[ForStatement], classOf[DoStatement], classOf[EnhancedForStatement])) {
  override val shortName = "LPC"
}
