package mains.generators.transformators

import corpus.CorpusData

// A class that handles things with special latex names for printing
abstract class LatexNamedTransformator(variableName: String = "(\\cdot)") extends CorpusTransformator {
  def getLatexHead(variableName: String): String
  override def toString = s"$$${getLatexHead(variableName)}$$"
  def andThen(f: LatexNamedTransformator): LatexNamedTransformator = {
    new LatexNamedTransformator(this.getLatexHead(variableName).replaceAll("\\$", "")) {
      def getLatexHead(variableName: String) = f.getLatexHead(variableName)
      def apply(data: CorpusData): CorpusData = f.apply(LatexNamedTransformator.this.apply(data))
    }
  }

}