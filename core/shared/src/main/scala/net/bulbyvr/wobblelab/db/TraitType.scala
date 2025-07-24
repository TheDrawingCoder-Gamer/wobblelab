package net.bulbyvr
package wobblelab.db

enum TraitType(val geneRepr: String) extends Enum[TraitType] {
  case Dom extends TraitType("AA")
  case Het extends TraitType("Aa")
  case Sub extends TraitType("aa")
}
