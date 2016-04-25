package ast

import org.eclipse.jdt.core.dom.{MethodDeclaration, VariableDeclaration}

// The whole points of this class is the two methods below
class MyMethodNode(override val node: MethodDeclaration, nodeString: String) extends RichASTNode[MethodDeclaration](node, nodeString) {
  lazy val localVariables: Seq[VariableDeclaration] = {
    collect {
      case e: VariableDeclaration => e
    }.toVector
  }

  lazy val localVariableNames: Seq[String] = localVariables.map(_.getName.toString)
}