package mains.runnable

import java.io.File

import alg.LempelZivForStrings
import il.ac.technion.cs.ssdl.java.{Token, Tokenizer}

import scala.collection.mutable

object LZTokenizer {

	def apply(f: File): (Int, Int) = {
		val t = new Tokenizer(f)
		val tokens = getTokens(t)
		tokens.size -> LempelZivForStrings(tokens).size
	}
	
	def getTokens(t: Tokenizer): Seq[String] = {

		def aux(t: Tokenizer, res: List[String] = Nil): List[String] = t.next match {
			case Token.EOF => res
			case Token.SPACE | Token.NL => aux(t, res)
			case default => aux(t, t.text() :: res)
		}
		aux(t).reverse
	}
	def main(args: Array[String]) {
		if (args.isEmpty)
			throw new RuntimeException("First argument to the program should be the path to a java file")
		val t = new Tokenizer(args.head)
		val tokens = getTokens(t)
		println(s"Ratio for ${args.head}: ${tokens.size.toDouble / LempelZivForStrings(tokens).size}")

	}
}
