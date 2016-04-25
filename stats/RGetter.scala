package stats

import common.os.RichOs
import org.ddahl.jvmr.RInScala
import common.os.RichWindows
import common.os.RichWindows

/**
 * A wrapper for the OS-specific location of the R executable
 * (could perhaps be solved using environment variables?)
 */
object RGetter {
  private val os = System.getProperty("os.name")
  lazy val get: RInScala = {
    if (os.toLowerCase.contains("windows"))
      RInScala( """C:\dev\lang\R\R-3.0.2\bin\x64\R.exe""")
    else if (os.toLowerCase.contains("linux"))
      RInScala( """/usr/bin/R""")
    else
      throw new MatchError(os)
  }
}