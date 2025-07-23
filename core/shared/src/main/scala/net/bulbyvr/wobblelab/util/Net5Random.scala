package net.bulbyvr.wobblelab.util

class Net5Random(seed: Int):
  val prng = Net5Random.CompactPrng(seed)

  def sample(): Double = prng.sample()

  def next(): Int = prng.internalSample()
  def next(maxValue: Int): Int = (prng.sample() * maxValue).toInt
  def next(minValue: Int, maxValue: Int): Int =
    val range = maxValue.toLong - minValue
    assert(range <= Int.MaxValue)
    ((prng.sample() * range) + minValue).toInt

object Net5Random:
  class CompactPrng(seed: Int):
    val seedArray: Array[Int] = Array.fill(56)(0)

    locally:
      val subtraction = if seed == Int.MinValue then Int.MaxValue else math.abs(seed)
      var mj: Int = 161803398 - subtraction
      seedArray(55) = mj
      var mk: Int = 1

      var ii: Int = 0
      (1 until 55).foreach: i =>
        ii += 21
        if ii >= 55 then
          ii -= 55

        seedArray(ii) = mk
        mk = mj - mk
        if mk < 0 then
          mk += Int.MaxValue

        mj = seedArray(ii)
      (1 until 5).foreach: k =>
        (1 until 56).foreach: i =>
          var n = i + 30
          if n >= 55 then
            n -= 55

          seedArray(i) -= seedArray(1 + n)
          if seedArray(i) < 0 then
            seedArray(i) += Int.MaxValue


    var inext: Int = 0
    var inextp = 21

    def internalSample(): Int =
      var locINext = inext + 1
      if locINext >= 56 then
        locINext = 1

      var locINextp = inextp + 1
      if locINextp >= 56 then
        locINext = 1

      var retVal = seedArray(locINext) - seedArray(locINextp)
      if retVal == Int.MaxValue then
        retVal -= 1
      if retVal < 0 then
        retVal += Int.MaxValue

      seedArray(locINext) = retVal
      inext = locINext
      inextp = locINextp

      retVal

    def sample(): Double =
      internalSample() * (1.0 / Int.MaxValue)

