package net.bulbyvr.magic

import deriving.Mirror
import compiletime.erasedValue
import scala.runtime.FunctionXXL
type Curried[Tup <: Tuple, R] = Tup match
  case h *: t => h => Curried[t, R]
  case EmptyTuple.type => R
inline def curried[T](f: Any)(using mpo: Mirror.ProductOf[T]) : Curried[mpo.MirroredElemTypes, T] =
  def c[Tup <: Tuple, R](f: Tup => R): Curried[Tup, R] = inline erasedValue[Tup] match
    case _: (h *: t) => (x: h) => c[t, R]((y: t) => f((x *: y).asInstanceOf))
    case _: EmptyTuple.type => f(EmptyTuple.asInstanceOf)
  c((tup : mpo.MirroredElemTypes) => f.asInstanceOf[FunctionXXL].apply(tup.toIArray).asInstanceOf[T]) 
import io.github.arainko.ducktape.function.FunctionMirror
import io.github.arainko.ducktape.function.FunctionMirror.given

// inline def foo[F](fun : F)(using f : FunctionMirror[F])(using mpo : Mirror.ProductOf[F]) : Curried[mpo.MirroredElemTypes, f.Return] = 
  
extension (self : FunctionXXL)(using f : FunctionMirror[self.type])(using mpo : Mirror.ProductOf[self.type])
  inline def tupled : mpo.MirroredElemTypes =>  f.Return = 
    (tup : mpo.MirroredElemTypes) => self(tup.toIArray).asInstanceOf[f.Return]
  inline def curried : Curried[mpo.MirroredElemTypes, f.Return] = 
    def c[Tup <: Tuple, R](fun : Tup => R) : Curried[Tup, R] = inline erasedValue[Tup] match 
      case _ : (h *: t) => (x : h) => c[t, R]((y : t) => fun((x *: y).asInstanceOf))
      case _ : EmptyTuple.type => fun(EmptyTuple.asInstanceOf)
    c(self.tupled)
     
    
   
