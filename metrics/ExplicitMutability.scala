package metrics

import org.eclipse.jdt.core.dom._

// fields that aren't final aren't immutable
object ExplicitMutability extends CountingMetric {

  override protected def f: PartialFunction[ASTNode, Boolean] = {
    case e: FieldDeclaration => !Modifier.isFinal(e.getModifiers)
  }
  
  override val shortName = "MTE"
}
