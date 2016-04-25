package corpus

trait FileCommitValue[T, Repr] {
  self: FileCommit =>
  protected val value: T
  def map[B, That](f: T => B)(implicit bf: (Repr, B) => That): That = withValue(f(value))(bf)
  def withValue[B, That](newValue: B)(implicit bf: (Repr, B) => That): That = bf(this.asInstanceOf[Repr], newValue)

  override def toString = (value, file, revision).toString
}
