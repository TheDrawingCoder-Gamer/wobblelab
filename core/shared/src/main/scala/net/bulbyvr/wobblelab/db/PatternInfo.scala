package net.bulbyvr.wobblelab.db

case class StripePatternInfo(a: Int, b: Int, c: Float, d: Float, e: Float, f: Float)
case class RepeatingPatternInfo(a: Int, b: Int, c: Float, d: Float, e: Float, f: Float)
case class SplotchPatternInfo(a: Int, b: Int, c: Float, d: Float, e: Float, f: Float)

case class PatternInfo(splotchInfo: SplotchPatternInfo, stripeInfo: StripePatternInfo, repeatingPatternInfo: RepeatingPatternInfo)
