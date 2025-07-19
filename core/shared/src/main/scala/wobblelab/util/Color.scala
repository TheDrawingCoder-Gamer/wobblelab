package net.bulbyvr
package wobblelab.util

case class Color(r: Int, g: Int, b: Int, a: Int = 255):
  override def toString: String =
    // web color is rgba
    f"#$r%02x$g%02x$b%02x$a%02x"
case class ColorF(r: Float, g: Float, b: Float, a: Float = 1.0):
  def toIntColor: Color =
    Color((r * 255).toInt, (g * 255).toInt, (b * 255).toInt, (a * 255).toInt)

  override def toString: String = toIntColor.toString
object ColorF {
  val WHITE: ColorF = ColorF(1, 1, 1, 1)
}