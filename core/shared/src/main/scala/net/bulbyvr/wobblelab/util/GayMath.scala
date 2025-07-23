package net.bulbyvr.wobblelab.util

object GayMath:
  def clampf(value: Float, min: Float, max: Float): Float =
    math.max(math.min(value, max), min)
