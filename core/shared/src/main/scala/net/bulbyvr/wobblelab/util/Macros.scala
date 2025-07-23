package net.bulbyvr.wobblelab.util

import compiletime.*
import scala.deriving.Mirror
import scala.quoted.*
import scala.{Symbol => _}

object Macros:
  private inline def summonSingletonsImpl[T <: Tuple, A](inline typeName: Any): List[A] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t) =>
        inline summonInline[Mirror.Of[h]] match
          // You may not like it, but m may be null
          case m: Mirror.Singleton => m.asInstanceOf[A] :: summonSingletonsImpl[t, A](typeName)
          case m: Mirror.SumOf[h] => summonSingletonsImpl[m.MirroredElemTypes, A](typeName) ++ summonSingletonsImpl[t, A](typeName)
          case m: Mirror => error("Non-singleton/sum type found: " + constValue[m.MirroredLabel])

  inline def summonSingletons[T](using s: Mirror.SumOf[T]): List[T] = summonSingletonsImpl[s.MirroredElemTypes, T](constValue[s.MirroredLabel])
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

  def fieldsOfObjOfKindImpl[T: Type](parent: Expr[Any])(using Quotes): Expr[List[T]] =
    import quotes.reflect.*

    val owner = parent.asTerm.tpe.typeSymbol
    val ownerTerm = parent.asTerm
    
    // Objects count as fields
    val decls: List[Symbol] = owner.declaredFields
    val kindDecls: List[Symbol] = decls.filter: it =>
      it.typeRef <:< TypeRepr.of[T]

    Expr.ofList(kindDecls.map(it => Select(ownerTerm, it).asExprOf[T]))


  inline def fieldsOfObjOfKind[T](obj: Any): List[T] =
    ${ fieldsOfObjOfKindImpl[T]('obj) }