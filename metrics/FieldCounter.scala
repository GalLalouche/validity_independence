package metrics

import org.eclipse.jdt.core.dom._

object FieldCounter extends SetCountingMetric(Set(classOf[FieldDeclaration])) {
  override val shortName = "NIV"
}
