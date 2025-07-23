package net.bulbyvr.wobblelab.util

case class LinearCurve(points: List[(Float, Float)]):
  def evaluate(x: Float): Float =
    val (hx, hy) = points.head
    if x < hx then
      hy
    else
      points.iterator.sliding(2).withPartial(false).find {
        case Seq((minx, miny), (maxx, maxy)) => x >= minx && x <= maxx
      }.map {
        case Seq((minx, miny), (maxx, maxy)) =>
          val percentOfX = (x - minx) * (maxx - minx)
          (percentOfX * (maxy - miny)) + miny
      }.getOrElse(points.last._2)

object LinearCurve:
  def apply(points: (Float, Float)*): LinearCurve =
    LinearCurve(points.toList)