package net.bulbyvr.magic

import deriving.Mirror
import compiletime.erasedValue
import scala.runtime.FunctionXXL
type Curried[Tup <: Tuple, R] = Tup match
  case (h *: t) => h => Curried[t, R]
  case EmptyTuple => R
import io.github.arainko.ducktape.function.FunctionMirror
import io.github.arainko.ducktape.function.FunctionMirror.given
import compiletime.error 
import annotation.experimental 
@experimental
object FunctionHelper {
  import util.TupledFunction
  def tupled[F, G](fun : F)(using f : TupledFunction[F, G]) : G = 
    f.tupled(fun)

  inline def curried[F, T <: Tuple, R](fun : F)(using tf : TupledFunction[F, T => R])(using fm : FunctionMirror[F]) : Curried[T, R] = 
    def c[Tup <: Tuple, R](f : Tup => R) : Curried[Tup, R] = inline erasedValue[Tup] match 
      case _ : (h *: t) => 
        (x : h) => c[t, R]((y : t) => f((x *: y).asInstanceOf))
      case _ : EmptyTuple.type => f(EmptyTuple.asInstanceOf)
    c(tupled(fun))
}

