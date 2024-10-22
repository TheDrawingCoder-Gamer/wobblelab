package net.bulbyvr
package wobblelab.db

case class DomRecGene(shared: DomRecGeneStatic, value: TraitType) {
  def currentProperty: DomRecGeneProperty
  = {
    value match
      case TraitType.Dom => shared.dom
      case TraitType.Het => shared.het
      case TraitType.Sub => shared.sub
  }
}
