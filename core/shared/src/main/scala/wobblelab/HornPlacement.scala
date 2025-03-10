package net.bulbyvr
package wobblelab

enum HornPlacement(val display: String) {
  case HornPlacementNone extends HornPlacement("Standard")
  case HornPlacementCenter extends HornPlacement("Center")
  case HornPlacementTraditional extends HornPlacement("Traditional")
}