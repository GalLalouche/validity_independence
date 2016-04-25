package metrics

import java.lang.reflect.Modifier

import ast.RichASTNode
import org.eclipse.jdt.core.dom._

import scala.annotation.tailrec

object Chameleonicity extends CountingMetric {

  private[metrics] implicit class RichType(t: Type) {
    def getRealFullyQualifiedName: String = t match {
      case t: SimpleType =>
        val name = t.getName.getFullyQualifiedName
        if (name.contains(".")) name else "java.lang." + name
      case _ => t.toString
    }

    private def isFinal(e: SimpleType): Boolean =
      try Modifier.isFinal(Class.forName(new RichType(e).getRealFullyQualifiedName).getModifiers)
      catch {
        case e: ClassNotFoundException ⇒ false
        case e: NoClassDefFoundError ⇒ false
      } // assume non-final by default

    def isPolymorphic: Boolean = {
      @tailrec
      def aux(t: Type): Boolean = t match {
        case _ if t.isPrimitiveType ⇒ false
        case e: SimpleType ⇒ isFinal(e) == false
        case e: ArrayType ⇒ aux(e.getComponentType)
        case e: ParameterizedType ⇒ aux(e.getType)
        case e: QualifiedType ⇒ aux(e.getQualifier)
        case e: WildcardType ⇒ true
      }
      aux(t)
    }
  }

  override protected def f: PartialFunction[ASTNode, Boolean] = {
    case e: SingleVariableDeclaration => e.getType.isPolymorphic
  }
  override val shortName = "CHAM"
}
