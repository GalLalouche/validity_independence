package mains.generators.tables

import java.io.File

import common.rich.RichT.richT
import common.rich.path.RichFile.richFile
import mains.generators.LatexGenerator
import mains.generators.tables.LatexTableGenerator._

object LatexTableGenerator {

  implicit class richAny(xs: Any) {
    def toLatexString: String = xs match {
      case s: String if (s startsWith "\\") || (s startsWith "$") || (s startsWith "{") => s
      case s: String if !s.matches( """\d+(\.\d*)?([eE][+-]\d+)?""") => s"{$s}" // needed for SIunitx
      case e: Int => e.toString
      case d: Double if Math.abs(d) >= 100 => f"$d%.1e" // scientific-notation with E
          .replaceAll("[^\\d]0\\d+|(e?)", "$1")
          .replaceAll("e", "E")
      case d: Double if Math.abs(d) < 0.0000001 => "0" // 0.0 (or near zero) as 0
      case d: Double if Math.abs(d) >= 0.01 ⇒ // single digit precision
        f"$d%.2f"
      case d: Double => f"$d%.1e" // scientific-notation with E
          .replaceAll("\\.\\d*(e[+-]?\\d+)?", "$1")
          .replaceAll("(e[+-])0+(.*?)", "$1$2")
          .replaceAll("e", "E")
      case e: Any ⇒ e.toString
    }
  }

}

trait LatexTableGenerator extends LatexGenerator {
  protected val formatTableItem: PartialFunction[Any, Any] = PartialFunction.empty
  protected final def toLatexString(x: Any) =
    formatTableItem.lift(x).getOrElse(x).toLatexString
  implicit class richLatexDouble(d: Double) {
    val c = "*"
    def significance = d.mapTo {
      case d: Double if d < 0.001 ⇒ c * 3
      case d: Double if d < 0.01 ⇒ c * 2
      case d: Double if d < 0.05 ⇒ c
      case d: Double ⇒ ""
    }
  }
  implicit def richSeqDouble(xs: Seq[Any]) = new {
    lazy val toLatexLine: String =
      xs.map(toLatexString).mkString(" & ") + " \\\\"
    lazy val toLatexSimple: String = {
      val c = "*"
      xs
          .map({
            case d: Double if d < 0.001 ⇒ c * 3
            case d: Double if d < 0.01 ⇒ c * 2
            case d: Double if d < 0.05 ⇒ c
            case d: Double ⇒ ""
            case e: Any ⇒ e.toString
          })
          .mkString(" & ") + "\\\\"
    }
  }


  // overloads are used instead of default parameters to allow invocations without ()
  // TODO: remove
  protected final def appendMidRule {appendMidRule(file)}
  protected final def appendMidRule(file: File) = file appendLine "\\midrule"
  protected final def appendTopRule {appendTopRule(file)}
  protected final def appendTopRule(file: File) = file appendLine "\\toprule"
  protected final def appendBottomRule {appendBottomRule(file)}
  protected final def appendBottomRule(file: File = file) = file appendLine "\\bottomrule"

  def writeLinesToLatex(xss: Seq[Seq[Any]], file: File = file) {
    xss.foreach(writeLineToLatex(_, file))
  }
  def writeLineToLatex(line: Seq[Any], file: File = file) {
    file.appendLine(line.toLatexLine.log())
  }
}
