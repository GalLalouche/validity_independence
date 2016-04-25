package mains

import il.ac.technion.cs.ssdl.java.{Token, Tokenizer}
import common.rich.RichAll._

import scala.annotation.tailrec

object HalsteadCalculator {
	case class Measures(val vocabulary: Int, length: Int, calculatedLength: Double, volume: Double, difficulty: Double, effort: Double) {
		private def isValidDouble(d: Double) = d.isRealDouble && d > 0
		require(vocabulary > 0)
		require(length > 0)
		require(isValidDouble(calculatedLength))
		require(isValidDouble(volume))
		require(isValidDouble(difficulty))
		require(isValidDouble(effort))
	}
	private object Measures {
		private def lg(d: Double) = Math log d / lg2
		private val lg2 = Math.log(10)
		def apply(distinctOperators: Int, distinctOperands: Int, totalOperators: Int, totalOperands: Int) = {
			require(distinctOperands > 0)
			require(totalOperators > 0)
			require(distinctOperands > 0)
			require(totalOperands > 0)
			val vocabulary = distinctOperands * distinctOperators
			val length = totalOperands + totalOperators
			val calculatedLength = distinctOperands * lg(distinctOperands) + distinctOperators * lg(distinctOperators)
			val volume = length * lg(vocabulary)
			val difficulty = (distinctOperators / 2.0) * (totalOperators / distinctOperators)
			val effort = difficulty * volume
			new Measures(vocabulary = vocabulary, length = length, calculatedLength = calculatedLength,
				volume = volume, difficulty = difficulty, effort = effort)
		}
	}
	def getTokens(tokenizer: Tokenizer): Seq[(Token, String)] = {
		@tailrec
		def aux(tokenizer: Tokenizer, res: List[(Token, String)] = Nil): List[(Token, String)] = {
			val token = tokenizer.next
			if (token == Token.EOF)
				res
			else
				aux(tokenizer, (token -> tokenizer.text) :: res)
		}
		aux(tokenizer).reverse.toVector
	}
	def apply(f: java.io.File): Measures = {
		val t = new Tokenizer(f)
		val (operands, operators) = getTokens(t)
			.filterNot(_._1.mapTo(e => e == Token.SPACE || e.isNL))
			.partition(_._1 == Token.IDENTIFIER).map(_.map(_._2))
		Measures(totalOperators = operators.size,
			totalOperands = operands.size,
			distinctOperators = operators.toSet.size,
			distinctOperands = operands.toSet.size)
	}
}
