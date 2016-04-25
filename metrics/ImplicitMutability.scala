//package metrics
//
//import ast.RichASTNode
//import ast.RichASTNode._
//import org.eclipse.jdt.core.dom._
//
//import scala.collection.JavaConversions._
//import scala.collection.mutable
//
//// only fields that are changed outside of Ctors are mutable
//object ImplicitMutability extends Metric {
//  private def isConstructor(md: MethodDeclaration): Boolean = {
//    md.toRichNode
//      .parents
//      .find(_.isInstanceOf[TypeDeclaration])
//      .map(_.asInstanceOf[TypeDeclaration].getName.toString).get == md.getName.toString
//  }
//
//  override def apply(myNode: RichASTNode[_ <: ASTNode]): Double = {
//    var x = 0
//    // marks all fields that need a second pass to see if there are changed later
//    // fields that can already be decided on (final / non-private) are counted
//    val privateNoneFinalFields = mutable.HashSet[String]()
//    val privateNoneFinalFieldsThatWereLaterSet = mutable.HashSet[String]()
//    def firstPhase() {
//      myNode.visit {
//        case e: FieldDeclaration =>
//          val m = e.getModifiers
//          if (Modifier.isFinal(m))
//            ()
//          else if (Modifier.isPrivate(m) == false)
//            x += 1
//          else
//            (e.fragments.map(_.asInstanceOf[VariableDeclarationFragment]).map(_.getName.toString)).foreach(privateNoneFinalFields.add)
//      }
//    }
//    firstPhase()
//    def secondPhase() {
//      myNode.visit {
//        case e: Assignment => e.getLeftHandSide match {
//          case name: SimpleName if privateNoneFinalFields.contains(name.getIdentifier) => {
//            if (false == e.toRichNode.parents.exists({
//              case md: MethodDeclaration if (isConstructor(md)) => true
//              case _ => false
//            }))
//              privateNoneFinalFieldsThatWereLaterSet += name.getIdentifier
//          }
//        }
//      }
//    }
//
//    secondPhase()
//    privateNoneFinalFieldsThatWereLaterSet.size + x
//  }
//
//
//  override def toString = "ImplicitMutability"
//
//  override val shortName = "MTI"
//}
