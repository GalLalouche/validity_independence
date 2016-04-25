package metrics

import ast.RichASTNode
import common.rich.RichT._
import org.eclipse.jdt.core.dom._

class Sha1 private(modulo: Int) extends Metric {
	import metrics.Sha1.sha1

	private def hex2dec(hex: String): BigInt =
		hex.toLowerCase.toList
			.map("0123456789abcdef".indexOf(_))
			.map(BigInt(_))
			.reduceLeft(_ * 16 + _)

	override def apply(myNode: RichASTNode[_ <: ASTNode]): Double =
		sha1.digest(myNode.nodeString.getBytes("UTF-8"))
			.map(e => f"$e%02x").mkString
			.mapTo(hex2dec)
			.mapTo(_ % modulo).toDouble

	override val shortName: String = "SHA1"
}

object Sha1 {
	private val sha1 = java.security.MessageDigest.getInstance("SHA-1")
	def apply(modulo: Int) = new Sha1(modulo)
}
