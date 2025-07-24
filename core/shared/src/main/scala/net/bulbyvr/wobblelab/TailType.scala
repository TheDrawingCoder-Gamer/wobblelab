package net.bulbyvr
package wobblelab

enum TailType(val display: String) {
  case NoTail extends TailType("None")
  case Stiff extends TailType("Stiff")
  case StiffCurly extends TailType("Curly")
  case Flowy extends TailType("Flowy")
  case Nub extends TailType("Nub")
  case StiffSlightlyCurly extends TailType("Arced")
  case Bulbous extends TailType("Bulbous")
  case Feral extends TailType("Feral")
  case Lifted extends TailType("Upright")
  case Paddle extends TailType("Paddle")
  case Plume extends TailType("Plume")
  case Whip extends TailType("Whip")
  case Curl extends TailType("Fancy")
  case DoubleCurl extends TailType("Bun")
  case Tri extends TailType("Tri")
}

given util.PrettyPrint[TailType] = _.display