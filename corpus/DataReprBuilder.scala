package corpus

/**
* @SuperRepr The type of parent type of all representers
* @SuperElem The type of parent type of all elements that the representers are composed of
* @Repr The concrete type of the representer
* @Elem The concrete type of the elements `Repr` is made of
* @SuperRepr A "subtype" of `Elem` (actually, a type that `Elem` wraps), that `Elem` can be built from
*/
trait DataReprBuilder[SuperRepr, SuperElem, Repr <: SuperRepr, Elem <: SuperElem, SubElem] extends
ComposedSequence[SubElem, Repr] {
  protected val repr: Repr
  protected def extractElements(repr: Repr): Seq[Elem]
  protected def elements: Seq[Elem] = extractElements(repr)

  protected def buildFromSuperElement(e: Elem, se: SuperElem): Elem
  protected def buildFromSuperElements(ses: Seq[SuperElem]): Repr =
    buildFromSuperElements(ses, repr)
  protected def buildFromSuperElements(ses: Seq[SuperElem], repr: SuperRepr): Repr =
    buildRepr(repr, elements zip ses map (e => buildFromSuperElement(e._1, e._2)))
  protected def buildFromSubElement(e: Elem, se: SubElem): Elem
  protected def buildRepr(mm: SuperRepr, xs: Seq[Elem]): Repr

  override protected def buildFromData(data: Seq[SubElem]) =
    buildRepr(repr, elements zip data map (e => buildFromSubElement(e._1, e._2)))
}
