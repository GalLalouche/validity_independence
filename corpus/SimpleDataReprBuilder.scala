package corpus

// doesn't have SubElem
trait SimpleDataReprBuilder[SuperRepr, SuperElem, Repr <: SuperRepr, Elem <: SuperElem]
  extends DataReprBuilder[SuperRepr, SuperElem, Repr, Elem, Elem] {

  override protected def buildFromSubElement(e: Elem, fc: Elem) = buildFromSuperElement(e, fc)
  override protected def data = extractElements(repr)
}
