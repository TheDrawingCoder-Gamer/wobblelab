package net.bulbyvr
package wobblelab

enum WingType(val display: String, val withFeathers: Boolean = false) {
  case NoWings extends WingType("None")
  case Angel extends WingType("Fluffy", withFeathers = true)
  // real name?
  case Bat extends WingType("Bat")
  // real name?
  case Paradise extends WingType("Paradise", withFeathers = true)
  case Vestigial extends WingType("Insect")
  // real name?
  case Vulture extends WingType("Vulture", withFeathers = true)
}

given util.PrettyPrint[WingType] = _.display