package corpus

/**
* This class allows one to compose partial functions using orElse
*/
class ComposablePartialFunctionality[-A, +B] private(currentPf: PartialFunction[A, B]) extends PartialFunction[A, B] {
  def this() = this(new PartialFunction[A, B] {
    override def isDefinedAt(x: A): Boolean = false
    override def apply(v1: A): B = throw new MatchError(v1)
  })
  def ::[C <: A, D >: B](newPf: PartialFunction[C, D]): ComposablePartialFunctionality[C, D] =
    new ComposablePartialFunctionality[C, D](newPf orElse ComposablePartialFunctionality.this.currentPf)
  def prepend[C <: A, D >: B](newPf: PartialFunction[C, D]): ComposablePartialFunctionality[C, D] = ::(newPf)
  override def apply(v1: A): B = currentPf apply v1
  override def isDefinedAt(x: A): Boolean = currentPf isDefinedAt x
}
