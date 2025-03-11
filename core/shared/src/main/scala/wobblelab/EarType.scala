package net.bulbyvr
package wobblelab

enum EarType(val display: String) {
  case TypeA extends EarType("Capsule")
  case TypeB extends EarType("TypeB")
  case Blunt extends EarType("Blunt")
  case Bent extends EarType("Bent")
  case Bulbous extends EarType("Bulbous")
  case Horn extends EarType("Horn")
  case Cross extends EarType("Cross")
  case Twisted extends EarType("Twisted")
  case Shepherd extends EarType("Shepherd")
  case Wavy extends EarType("Wavy")
}