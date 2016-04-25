package metrics

import org.eclipse.jdt.core.dom._

object BranchCounter extends SetCountingMetric(Set(classOf[WhileStatement], classOf[ForStatement], classOf[DoStatement],
  classOf[IfStatement], classOf[SwitchCase], classOf[ConditionalExpression], classOf[EnhancedForStatement])) {
  override val shortName = "BRC"
}
