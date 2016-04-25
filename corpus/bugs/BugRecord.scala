package corpus.bugs

import java.io.File

import ast.RichASTNode
import org.eclipse.jdt.core.dom.ASTNode

class BugRecord(val node: RichASTNode[ASTNode], val file: File, val revision: String)