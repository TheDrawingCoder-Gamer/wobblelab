package net.bulbyvr
package wobblelab

enum TailType(val display: String) {
  case NoTail extends TailType("None")
  case Stiff extends TailType("Stiff")
  case StiffCurly extends TailType("Stiff Curly")
  case Flowy extends TailType("Flowy")
  case Nub extends TailType("Nub")
  case StiffSlightlyCurly extends TailType("Stiff Slightly Curly")
  case Bulbous extends TailType("Bulbous")
  case Feral extends TailType("Feral")
  case Lifted extends TailType("Lifted")
  case Paddle extends TailType("Paddle")
  case Plume extends TailType("Plume")
  case Whip extends TailType("Whip")
  case Curl extends TailType("Curl")
  case DoubleCurl extends TailType("Double Curl")
  case Tri extends TailType("Tri")
}