package corpus

import common.rich.RichAll._

/**
 * A composed sequence is any element that has a composition of a sequence as its main attribute.
 * This is a case of favouring composition over inheritance (which we do by inheriting this trait, teehee)
 */
trait ComposedSequence[Elem, Repr] {
	protected def data: Seq[Elem]
	protected def buildFromData(data: Seq[Elem]): Repr

	def map(f: Elem => Elem): Repr = buildFromData(data map f)
	def flatMap(f: Seq[Elem] => Seq[Elem]) = buildFromData(f(data))
	def filter(f: Elem => Boolean): Repr = buildFromData(data filter f)
	def filterNot(f: Elem => Boolean): Repr = filter(e => !f(e))
	def length = data.size
	def apply(index: Int): Elem = data(index)
	def isEmpty = data.isEmpty
	def nonEmpty = data.nonEmpty
	def take(n: Int) = buildFromData(data take n)
	def drop(n: Int) = buildFromData(data drop n)
	def head = this take 1
	def tail = buildFromData(data tail)
	def last = buildFromData(List(data last))
	def shuffle = buildFromData(data shuffle)
	def find(e: Elem): Option[Elem] = data find e
}
