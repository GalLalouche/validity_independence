package ast

import org.eclipse.jdt.core.dom._
import common.rich.path.RichFile._
import java.io.File

object ASTStringParser {
  
  def parse(str: String): RichASTNode[CompilationUnit] = {
    val parser = ASTParser.newParser(AST.JLS3)
    parser.setKind(ASTParser.K_COMPILATION_UNIT)
    parser.setSource(str.toCharArray)
    val tree = parser.createAST(null).asInstanceOf[CompilationUnit]

    new RichASTNode[CompilationUnit](tree, str)
  }
  def parse(f: File): RichASTNode[CompilationUnit] = parse(f.readAll)

  private def resolveBinding(parser: org.eclipse.jdt.core.dom.ASTParser) {
    parser.setStatementsRecovery(true)
    parser.setResolveBindings(true)
    parser.setEnvironment(null, null, null, true)
    parser.setUnitName("foo.java")
  }
}

