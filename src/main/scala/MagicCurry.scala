package net.bulbyvr.magic

import deriving.Mirror
import compiletime.erasedValue
import scala.runtime.FunctionXXL
type Curried[Tup <: Tuple, R] = Tup match
  case (h *: t) => h => Curried[t, R]
  case EmptyTuple => R
import compiletime.error 
import annotation.experimental 
import annotation.implicitNotFound
@experimental
object FunctionHelper {
  import util.TupledFunction
  def tupled[F, G](fun : F)(using f : TupledFunction[F, G]) : G = 
    f.tupled(fun)
  inline private def curryTuple[Tup <: Tuple, R](f : Tup => R) : Curried[Tup, R] = inline erasedValue[Tup] match
    case _ : (h *: t) => 
      (x : h) => curryTuple[t, R]((y : t) => f((x *: y).asInstanceOf))
    case _ : EmptyTuple => f(EmptyTuple.asInstanceOf)

  inline def curried[F, T <: Tuple, R](fun : F)(using tf : TupledFunction[F, T => R]) : Curried[T, R] = 
        curryTuple(tupled(fun))
}
/*
@experimental 
object ApplicativeHelper {
  import cats.syntax.all._
  import cats.Applicative
  import util.TupledFunction
  import quoted.*
  inline def mapN[F, T <: Tuple, A[_], R](tuple: T)(fun : F)(using tf : TupledFunction[F, T => R])(using app : Applicative[A]) : A[R] = 
    FunctionHelper.curried(fun).pure[A]

  private def apTuple[H, T <: Tuple, A[_], R](tuple: A[H] *: T)(fun : A[Curried[H *: T, R]])(using app : Applicative[A]) : (A[Curried[T, R]], T) =
    tuple match 
      case (h *: t) => 
        (fun.ap(h), t)
  inline private def apN[H, T <: Tuple, A[_], R](tuple: A[H] *: T)(fun : A[Curried[A[H] *: T, R]])(using app : Applicative[A]) : A[R] = 
    tuple match 
      case (h *: t) : (A[H] *: (A[_] *: Tuple)) => 
        apN(t)(fun.ap(h))

      case (h *: t) : (A[H] *: EmptyTuple) => 
        fun.ap(h)
        
}
*/
import cats.Apply
import cats.syntax.all._
extension [T <: NonEmptyTuple](self : T)
  def tupled[F[_]](using Apply[F])(using Tuple.IsMappedBy[F][T]): F[Tuple.InverseMap[T, F]] = 
    def loop[X <: NonEmptyTuple](x : X): F[NonEmptyTuple] = x match 
      case hd *: EmptyTuple =>  hd.asInstanceOf[F[Any]].map(_ *: EmptyTuple)
      case hd *: (tl : NonEmptyTuple) => hd.asInstanceOf[F[Any]].map2(loop(tl))(_ *: _)
    loop(self).asInstanceOf[F[Tuple.InverseMap[T, F]]]
  def mapN[F[_], B](using Apply[F])(using Tuple.IsMappedBy[F][T])(f : Tuple.InverseMap[T, F] => B) : F[B] = 
    self.tupled.map(f)



