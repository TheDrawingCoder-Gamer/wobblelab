package net.bulbyvr.magic 

import scala.annotation.implicitNotFound

@implicitNotFound("FunctionMirrorXXLs are only available for functions, but got ${F}")
sealed trait FunctionMirrorXXL[F] {
  type Return
}

object FunctionMirrorXXL extends FunctionMirrorXXL[Any => Any] {
  type Aux[F, R] = FunctionMirrorXXL[F] {
    type Return = R 
  }

  override type Return = Any 

  transparent inline given [F]: FunctionMirrorXXL[F] = FunctionMacrosXXL.createMirror[F]
}
