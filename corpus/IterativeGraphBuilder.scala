package corpus

import ast.ASTStringParser
import il.ac.technion.cs.ssdl.sablecc.GitLogsRecord
import org.eclipse.jdt.core.dom._
import parsers.git.GitAccessor

import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._

class IterativeGraphBuilder private(val graph: Graph[String, DiEdge]) extends GitAccessor {
	def this() = this(Graph.empty)
	def addRevision(data: (String, Seq[GitLogsRecord])): IterativeGraphBuilder = addRevision(data._1, data._2)
	def addRevision(revision: String, files: Seq[GitLogsRecord]): IterativeGraphBuilder = {
		val data = files
			.map(e => getVersion(e.file, e.revision).get)
			.map(ASTStringParser.parse)
			.flatMap(_.collect { case e: TypeDeclaration => e})
			.filterNot(_.isInterface)
			.map(e => e.getName.toString -> Option(e.getSuperclassType).map(_.toString))
			.toMap
		var $ = graph
		for ((className, superClass) <- data) {
			if ($.contains(className)) { // remove old edge
				val outgoingEdges = $.get(className).outgoing
//				require(outgoingEdges.size <= 1, s"node $className has too many outgoing edges... (${outgoingEdges.map(_.target)}")
				for (e <- outgoingEdges.headOption)
					$ = $ - e
			} else // add new vertex
				$ += className

			for (superClass <- superClass)
				$ += className ~> superClass
		}
		new IterativeGraphBuilder($)
	}
}
