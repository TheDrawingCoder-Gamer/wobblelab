package net.bulbyvr
package wobblelab

enum HornPlacement(val display: String) {
  case HornPlacementNone extends HornPlacement("No horns")
  case HornPlacementCenter extends HornPlacement("Center")
  case HornPlacementTraditional extends HornPlacement("Traditional")
  case HornPlacementAll extends HornPlacement("Traditional + Center")
}