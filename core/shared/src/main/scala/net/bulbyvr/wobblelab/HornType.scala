package net.bulbyvr
package wobblelab

enum HornType(val display: String) {
  case NoHorns extends HornType("None")
  case Curled extends HornType("Curled")
  case Nub extends HornType("Nub")
  case Thick extends HornType("Thick")
  case Thin extends HornType("Thin")
}

given util.PrettyPrint[HornType] = _.display