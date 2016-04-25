package parsers.bugs

import corpus.FileCommit

/**
 * A bug parser is a function that matches a bug value to each file commit.
 * A value may be a None if there is no for the metric, or Some if there is a value
 */
trait BugParser[T] extends (FileCommit => Option[T])