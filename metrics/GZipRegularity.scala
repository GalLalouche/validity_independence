package metrics

import java.util.zip.Deflater

import ast.RichASTNode
import org.eclipse.jdt.core.dom.ASTNode

object GZipRegularity extends InverseMetric {
  def apply(v1: ASTNode): Double = throw new UnsupportedOperationException("please use apply(MyASTNode)")

  override def apply(v1: RichASTNode[_ <: ASTNode]) = {
    val code = v1.nodeString
    val output = new Array[Byte](code.length)
    val deflater = new Deflater
    deflater.setInput(code.getBytes)
    deflater.finish()
    val compressed: Double = deflater.deflate(output)
    compressed
  }

  override val shortName = "GZIP"
}
