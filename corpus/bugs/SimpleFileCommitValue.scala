package corpus.bugs

import corpus.{FileCommitValue, FileCommit}

/**
* Same as FileCommitValue, without messing with Repr and all of that.
* Provides a default implementation of bf
*/
class SimpleFileCommitValue[T](fc: FileCommit, val bugValue: T)
	extends FileCommit(fc)
	with FileCommitValue[T, SimpleFileCommitValue[T]] {
	protected val value = bugValue
}

object SimpleFileCommitValue {
	implicit def bf[T, B]: ((SimpleFileCommitValue[T], B) => SimpleFileCommitValue[B]) =
		(bv, newValue) => new SimpleFileCommitValue[B](bv, newValue)
}