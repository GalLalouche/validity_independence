package mains.generators

import common.rich.path.Directory
import common.rich.RichT.richT
import common.rich.path.RichFile.richFile
import mains.programs.Configuration
import mains.programs.Debug

/**
 * Base class for generators whose main output is a file
 */
class FileGenerator(_fileName: Option[String], extension: String) {
	def directory = Configuration.generated.addSubDir(extension)
	def fileName: String = _fileName.getOrElse(this.simpleName)

	def unclearedFile = directory.addFile(s"$fileName.$extension")

	private lazy val _file = unclearedFile.clear
	def file = _file
	override def finalize() {
		if (unclearedFile.lines.isEmpty)
			assert(unclearedFile.delete)
	}
	implicit lazy val tableFile = unclearedFile.clear
}
