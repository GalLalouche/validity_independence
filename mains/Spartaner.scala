package mains

import java.io.File

import common.rich.RichAll._
import design.TokenizerScala
import mains.programs.Debug
import mains.runnable.LZTokenizer

object Spartaner extends Debug {
	case class Corpus(str: String) {
		val files = List(Original, PartialSpartanization, FullSpartanization).map(_.getFile(str))
		for (f <- files) {
			LZTokenizer.main(Array(f.getAbsolutePath))
		}
	}

	case class Result(size: Int, compressionRatio: Double, sizeAfterZip: Int, ratio: Double, combinedRatio: Double)
	case class Source(javaSize: Int, lzwSize: Int, compressionRatio: Double)
	trait CorpusVersion {
		def getFile(c: String) = theify(new File(s"D:/tmp/${this.simpleName }/$c/big_file.java"))
	}

	object Original extends CorpusVersion
	object PartialSpartanization extends CorpusVersion
	object FullSpartanization extends CorpusVersion

	val corpora = List("hadoop-common",
		"elasticsearch",
		"atmosphere",
		"hbase",
		"netty",
		"docx4j",
		"cucumber-jvm",
		"guava",
		"voldemort",
		"guice",
		"titan",
		"CraftBukkit",
		"mongo-java-driver",
		"wildfly",
		"hazelcast",
		"jclouds",
		"jna",
		"lombok",
		"k-9",
		"hibernate-orm",
		"junit",
		"RxJava",
		"openmrs-core",
		"Essentials",
		"hector",
		"spring-framework").sortBy(_.toLowerCase)

	def theify(f: File): File = new File(f.parent, "big_file.wat")

	override def timedMain {
		for (t <- List(Original, PartialSpartanization, FullSpartanization)) {
			println(t.simpleName)
			println("--------")
			for (c <- corpora) {
				val file = t getFile c
				val (size, zipped) = LZTokenizer(file)
				println(s"$size\t $zipped")
			}
		}
	}
}
