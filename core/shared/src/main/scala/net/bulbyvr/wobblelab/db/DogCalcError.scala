package net.bulbyvr.wobblelab.db

enum DogCalcError:
  case ValueOutsideOfRange
  case Improper
  case NonIntegral
  case NonSuper
  case PercentOutsideOfRange(percent: Float)
  case NoBoundsForGene(prop: Gene)
  case NonExactIntegral