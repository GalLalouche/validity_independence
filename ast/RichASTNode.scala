package ast

import java.io.{ByteArrayInputStream, StringReader}

import il.ac.technion.cs.ssdl.java.{Token, Tokenizer}
import japa.parser.ast.body.{ConstructorDeclaration, MethodDeclaration}
import japa.parser.ast.visitor.VoidVisitorAdapter
import japa.parser.{JavaParser, ParseException, TokenMgrError}
import org.eclipse.jdt.core.dom.ASTNode._
import org.eclipse.jdt.core.dom._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

// _nodeString is lazy evaluated, to allow using empty strings in special cases,
// and assuring that on use of rich methods that require the nodeString, an exception will be thrown
class RichASTNode[+T <: ASTNode](val node: T, _nodeString: => String)
	extends Traversable[ASTNode] {
	require(node != null)
	lazy val nodeString: String = _nodeString

	override def toString = node.toString

	override def equals(o: Any) = node.equals(o)

	override def hashCode = node.hashCode

	private def visit(f: PartialFunction[ASTNode, Unit]) {
		node.accept(new ASTNodeVisitor() {
			override def visit(node: ASTNode) = {
				f(node)
				true
			}
		})
	}

	// caches the in ordered sequence of nodes, for performance
	private lazy val inOrderSeq: Seq[ASTNode] = {
		val buffer = ListBuffer[ASTNode]()
		node.accept(new ASTNodeVisitor() {
			override def visit(node: ASTNode) = {
				buffer += node
				true
			}
		})
		buffer
	}

	override def foreach[U](f: ASTNode => U) { inOrderSeq foreach f }

	lazy val isValid: Boolean = {
		try {
			this.methods.size
			true
		} catch {
			case e: japa.parser.ParseException =>
				println(s"failed to parse a file using Japa, skipping...")
				false
		}
	}

	lazy val methods: Traversable[MyMethodNode] = {
		val methodNameToSource = new collection.mutable.HashMap[String, String]
		try {
			JavaParser.parse(new ByteArrayInputStream(nodeString.getBytes("UTF-8")))
				.accept(new VoidVisitorAdapter[Any]() {
				override def visit(n: MethodDeclaration, arg: Any) {
					methodNameToSource(n.getName) = n.toString
				}

				override def visit(n: ConstructorDeclaration, arg: Any) {
					methodNameToSource(n.getName) = n.toString
				}
			}, null)
			val methodNameToAstNode = new collection.mutable.HashMap[String, org.eclipse.jdt.core.dom.MethodDeclaration]
			visit(PartialFunction[ASTNode, Unit] {
				case x: org.eclipse.jdt.core.dom.MethodDeclaration ⇒ methodNameToAstNode(x.getName.toString) = x
			})
			methodNameToAstNode
				.filter(x ⇒ methodNameToSource.contains(x._1))
				.map({
				case (methodName, astNode) ⇒ new MyMethodNode(astNode, methodNameToSource(methodName))
			})
		}
		catch {
			case pe: ParseException => Traversable.empty[MyMethodNode]
			case tme: TokenMgrError => Traversable.empty[MyMethodNode]
		}
	}

	lazy val typeString: String = node.getNodeType match {
		case ARRAY_ACCESS ⇒ "Array access"
		case ARRAY_CREATION ⇒ "Array creation"
		case ARRAY_INITIALIZER ⇒ "Array initializer"
		case ARRAY_TYPE ⇒ "Array type"
		case ASSIGNMENT ⇒ "Assignment"
		case BLOCK ⇒ "Block"
		case BLOCK_COMMENT ⇒ "Block comment"
		case BOOLEAN_LITERAL ⇒ "Boolean literal"
		case BREAK_STATEMENT ⇒ "Break statement"
		case CATCH_CLAUSE ⇒ "Catch clause"
		case CHARACTER_LITERAL ⇒ "Character literal"
		case CLASS_INSTANCE_CREATION ⇒ "Class instance creation"
		case CONDITIONAL_EXPRESSION ⇒ "Conditional expression"
		case CONSTRUCTOR_INVOCATION ⇒ "Constructor invocation"
		case CONTINUE_STATEMENT ⇒ "Continue statement"
		case DO_STATEMENT ⇒ "Do statement"
		case EMPTY_STATEMENT ⇒ "Empty statement"
		case ENHANCED_FOR_STATEMENT ⇒ "Enhanced for statement"
		case EXPRESSION_STATEMENT ⇒ "Expression statement"
		case FIELD_ACCESS ⇒ "Field access"
		case FIELD_DECLARATION ⇒ "Field declaration"
		case FOR_STATEMENT ⇒ "For statement"
		case IF_STATEMENT ⇒ "If statement"
		case IMPORT_DECLARATION ⇒ "Import declaration"
		case INFIX_EXPRESSION ⇒ "Infix expression"
		case INITIALIZER ⇒ "Initializer"
		case INSTANCEOF_EXPRESSION ⇒ "Instanceof expression"
		case LABELED_STATEMENT ⇒ "Labeled statement"
		case LINE_COMMENT ⇒ "Line comment"
		case MALFORMED ⇒ "Malformed"
		case MEMBER_REF ⇒ "Member ref"
		case MODIFIER ⇒ "Modifier"
		case NULL_LITERAL ⇒ "Null literal"
		case NUMBER_LITERAL ⇒ "Number literal"
		case PACKAGE_DECLARATION ⇒ "Package declaration"
		case PARENTHESIZED_EXPRESSION ⇒ "Parenthesized expression"
		case POSTFIX_EXPRESSION ⇒ "Postfix expression"
		case PREFIX_EXPRESSION ⇒ "Prefix expression"
		case PRIMITIVE_TYPE ⇒ "Primitive type"
		case QUALIFIED_NAME ⇒ "Qualified name"
		case QUALIFIED_TYPE ⇒ "Qualified type"
		case RETURN_STATEMENT ⇒ "Return statement"
		case SIMPLE_NAME ⇒ "Simple name"
		case SIMPLE_TYPE ⇒ "Simple type"
		case SINGLE_VARIABLE_DECLARATION ⇒ "Single variable declaration"
		case STRING_LITERAL ⇒ "String literal"
		case SUPER_CONSTRUCTOR_INVOCATION ⇒ "Super constructor invocation"
		case SUPER_FIELD_ACCESS ⇒ "Super field access"
		case SUPER_METHOD_INVOCATION ⇒ "Super method invocation"
		case SWITCH_CASE ⇒ "Switch case"
		case SWITCH_STATEMENT ⇒ "Switch statement"
		case TAG_ELEMENT ⇒ "Tag element"
		case TEXT_ELEMENT ⇒ "Text element"
		case THIS_EXPRESSION ⇒ "This expression"
		case THROW_STATEMENT ⇒ "Throw statement"
		case TRY_STATEMENT ⇒ "Try statement"
		case TYPE_DECLARATION ⇒ "Type declaration"
		case TYPE_DECLARATION_STATEMENT ⇒ "Type declaration statement"
		case TYPE_LITERAL ⇒ "Type literal"
		case VARIABLE_DECLARATION_EXPRESSION ⇒ "Variable declaration expression"
		case VARIABLE_DECLARATION_FRAGMENT ⇒ "Variable declaration fragment"
		case VARIABLE_DECLARATION_STATEMENT ⇒ "Variable declaration statement"
		case WHILE_STATEMENT ⇒ "While statement"
		case _ ⇒ "Other"
	}

	// lazy valed for performance, instead of returning a traversable
	lazy val parents: Seq[ASTNode] = {
		@tailrec // a tail recursion is its own reward!
		def aux(e: ASTNode, xs: ListBuffer[ASTNode] = ListBuffer[ASTNode]()): Seq[ASTNode] = {
			val parent = e.getParent
			if (parent != null)
				aux(e.getParent, xs += parent)
			else
				xs
		}
		aux(node)
	}

	lazy val tokens: Seq[String] = {
		val st = new Tokenizer(new StringReader(nodeString))
		var token = st.next
		val $ = mutable.MutableList[String]()
		while (token != Token.EOF) {
			if (token != Token.SPACE && token != Token.NL)
				$ += st.text
			token = st.next
		}
		$.toVector
	}

	lazy val isInterface: Boolean =  try this
		.find(_.isInstanceOf[TypeDeclaration])
		.get.asInstanceOf[TypeDeclaration].isInterface
		catch {
		  case _: NoSuchElementException => true // boring file
		}
}

object RichASTNode {
	japa.parser.JavaParser.setCacheParser(false)

	implicit def toNormalNode[T <: ASTNode](node: RichASTNode[T]): T = node.node

	implicit def toRichNodeRichMaker[T <: ASTNode](node: T) = new {
		def toRichNode = new RichASTNode[T](node, throw new UnsupportedOperationException("No node string given"))
	}
}
