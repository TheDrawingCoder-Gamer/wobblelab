package net.bulbyvr
package wobblelab.util

case class Color(r: Int, g: Int, b: Int, a: Int = 255):
  override def toString: String =
    // web color is rgba
    f"#$r%02x$g%02x$b%02x$a%02x"

  def showOpaque: String =
    f"#$r%02x$g%02x$b%02x"

  def toFloatColor: ColorF =
    ColorF(r.toFloat / 255f, g.toFloat / 255f, b.toFloat / 255f, a.toFloat / 255f)

object Color {
  def parseHex(n: String): Color =
    val x = if n.head == '#' then n.tail else n
    val num = Integer.parseUnsignedInt(x, 16)
    if x.length == 8 then
      Color((num >>> 24) & 255, (num >>> 16) & 255, (num >>> 8) & 255, num & 255)
    else
      Color((num >>> 16) & 255, (num >>> 8) & 255, num & 255)
}

case class ColorF(r: Float, g: Float, b: Float, a: Float = 1.0):
  def toIntColor: Color =
    Color((r * 255).toInt, (g * 255).toInt, (b * 255).toInt, (a * 255).toInt)

  def showOpaque: String = toIntColor.showOpaque

  override def toString: String = toIntColor.toString
object ColorF {
  val WHITE: ColorF = ColorF(1, 1, 1, 1)
}