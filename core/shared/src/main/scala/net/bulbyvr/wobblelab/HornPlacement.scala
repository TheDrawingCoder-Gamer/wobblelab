package net.bulbyvr
package wobblelab

enum HornPlacement(val display: String) {
  case None extends HornPlacement("Standard")
  case Center extends HornPlacement("Center")
  case Traditional extends HornPlacement("Traditional")
}

given util.PrettyPrint[HornPlacement] = _.display