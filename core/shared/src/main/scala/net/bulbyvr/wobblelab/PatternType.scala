package net.bulbyvr.wobblelab

enum PatternType:
  case None
  case Stripes
  case Splotches
  case Repeating
  
given util.PrettyPrint[PatternType] = _.toString