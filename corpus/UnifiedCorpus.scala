package corpus

import common.rich.path.Directory
/**
 * Represents a single Java project managed by GIT and residing in a single folder. This folder must include  a "log file" and
 * a GIT repository of the evolution of this project. The log file is expected to be the a GIT history file, which tells the story
 * of the evolution of each of the Java source files. Each file mentioned in the log must be be found in the repository. Similarly,
 * there should be at least one log entry for each of the source files. However, this later requirement is never checked.
 */
class UnifiedCorpus(c1: Corpus, c2: Corpus) extends Corpus(Directory("./Corpus"), (c1.index + c2.index).toChar) {
  override val fullName = s"${c1.index}U${c2.index}" 

}