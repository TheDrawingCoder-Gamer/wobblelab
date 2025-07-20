package net.bulbyvr.wobblelab.util

import compiletime.*
import scala.deriving.Mirror

object Macros:
  private inline def summonSingletonMapImpl[T <: Tuple, A](inline typeName: Any): List[(String, A)] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t) =>
        inline summonInline[Mirror.Of[h]] match
          case m: Mirror.Singleton => (constValue[m.MirroredLabel], m.fromProduct(EmptyTuple).asInstanceOf[A]) :: summonSingletonMapImpl[t, A](typeName)
          case m: Mirror =>
            error("Enum " + codeOf(typeName) + " contains non singleton case " + codeOf(constValue[m.MirroredLabel]))

  inline def summonSingletonMap[T](using s: Mirror.SumOf[T]): Map[String, T] = summonSingletonMapImpl[s.MirroredElemTypes, T](constValue[s.MirroredLabel]).toMap