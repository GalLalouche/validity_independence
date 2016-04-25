package corpus

import corpus.rich.RichData._
import il.ac.technion.cs.ssdl.sablecc.GitLogsRecord
import parsers.git.GitRecordsParser

import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge

class RevisionsGraph private(cache: Map[String, IterativeGraphBuilder], last: IterativeGraphBuilder) {
	def this() = this(Map(), new IterativeGraphBuilder())
	def apply(revision: String): Graph[String, DiEdge] = cache(revision).graph
	def addRevision(data: (String, Seq[GitLogsRecord])): RevisionsGraph = this.addRevision(data._1, data._2)
	def addRevision(revision: String, files: Seq[GitLogsRecord]): RevisionsGraph = {
		val newBuild = last.addRevision(revision, files)
		new RevisionsGraph(cache + (revision -> newBuild), newBuild)
	}
	def sample = cache.head._2.graph
}

object RevisionsGraph {
	def apply(c: Corpus): RevisionsGraph =
		GitRecordsParser
			.getLogs(c.logFile)
			.groupBy(_.revision)
			.toSeq
			.sortBy(_._2.head.timestamp)
			.foldLeft(new RevisionsGraph) { (g, e) => g addRevision e}
}
