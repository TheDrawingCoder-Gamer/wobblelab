package net.bulbyvr.wobblelab.util

import compiletime.*
import scala.deriving.Mirror

object Macros:  
  private inline def summonSingletonMapImpl[T <: Tuple, A](inline typeName: Any): List[(String, A)] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t) =>
        inline summonInline[Mirror.Of[h]] match
          // You may not like it, but m may be null
          case m: Mirror.Singleton => (constValue[m.MirroredLabel], m.asInstanceOf[A]) :: summonSingletonMapImpl[t, A](typeName)
          case m: Mirror.SumOf[h] =>
            summonSingletonMapImpl[m.MirroredElemTypes, A](typeName) ++ summonSingletonMapImpl[t, A](typeName)
          case m: Mirror => error("Non-singleton/sum type found: " + constValue[m.MirroredLabel])

  inline def summonSingletonMap[T](using s: Mirror.SumOf[T]): Map[String, T] = summonSingletonMapImpl[s.MirroredElemTypes, T](constValue[s.MirroredLabel]).toMap
