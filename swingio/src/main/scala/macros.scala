import cats.*
import cats.implicits.*
import fs2.concurrent.*
import cats.effect.*

import scala.quoted.*

object macros {
  def copyIMapMacro[F[_]: Type, S: Type, R: Type](base: Expr[SignallingRef[F, S]], fieldName: Expr[String], async: Expr[Async[F]])(using Quotes, Type[SignallingRef[?, ?]]): Expr[SignallingRef[F, R]] = {
    import quotes.reflect.*
    val daName = fieldName.valueOrAbort
    '{
      val goodbase = $base
      SignallingRef.lens[F, S, R](goodbase)(it => ${ Select.unique('it.asTerm, daName).asExprOf[R] }, src => it => ${ Select.overloaded('src.asTerm, "copy", List(), List(NamedArg(daName, 'it.asTerm))).asExprOf[S] } )(using $async)
    }
  }
  inline def copyIMap[F[_], S, R](inline base: SignallingRef[F, S], inline fieldName: String)(using Async: Async[F]): SignallingRef[F, R] = {
    ${ copyIMapMacro('base, 'fieldName, 'Async) }
  }
  extension[F[_]: Async, S](self: SignallingRef[F, S]) {
    inline def imapCopied[R](fieldName: String): SignallingRef[F, R] = {
      copyIMap(self, fieldName)
    }
  }
}
