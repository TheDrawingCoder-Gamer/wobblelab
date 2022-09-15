package net.bulbyvr.magic 
import quoted.*


class FunctionMacrosXXL(using val quotes: Quotes) {
  import quotes.reflect.*
  def createMirror[Func : Type] : Expr[FunctionMirrorXXL[Func]] = 
    TypeRepr.of[Func] match 
      case tpe @ AppliedType(_, tpeArgs) if tpe.isErasedFunctionType || tpe.isFunctionType => 
        val returnTpe = tpeArgs.last 
        returnTpe.asType match {
          case '[ret] => 
            '{
              FunctionMirrorXXL.asInstanceOf[
                FunctionMirrorXXL[Func] {
                  type Return = ret 
                }
              ]
            }
        }
      case other => report.errorAndAbort(s"FunctionMirrorXXLs can only be created for functions. Got ${other.show} instead.")
}

object FunctionMacrosXXL {
  def createMirrorMacro[Func : Type](using Quotes): Expr[FunctionMirrorXXL[Func]] = 
    FunctionMacrosXXL().createMirror[Func]
  transparent inline def createMirror[F] : FunctionMirrorXXL[F] = ${ createMirrorMacro[F] }
}
