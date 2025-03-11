package net.bulbyvr
package wobblelab

enum NoseType(val display: String) {
  case TypeA extends NoseType("Sphere")
  case Bulb extends NoseType("Bulb")
  case Greyhound extends NoseType("Greyhound")
  case HalfMallow extends NoseType("Half Mallow")
  case Mallow extends NoseType("Mallow")
  case Pug extends NoseType("Pug")
  case Square extends NoseType("Square")
  case Triangle extends NoseType("Triangle")
  case Wide extends NoseType("Wide")
}