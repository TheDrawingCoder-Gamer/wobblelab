package net.bulbyvr
package wobblelab
package db

import io.circe.*
import io.circe.Decoder.Result
import io.circe.generic.semiauto.*
import io.circe.syntax.*

import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonInline, error, codeOf}
import Predef.genericArrayOps

import net.bulbyvr.wobblelab.given

private [bulbyvr] inline def summonSingletonCases[T <: Tuple, A](inline typeName: Any): List[A] =
  inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (h *: t) =>
      inline summonInline[Mirror.Of[h]] match
        case m: Mirror.Singleton => m.fromProduct(EmptyTuple).asInstanceOf[A] :: summonSingletonCases[t, A](typeName)
        case m: Mirror =>
          error("Enum " + codeOf(typeName) + " contains non singleton case " + codeOf(constValue[m.MirroredLabel]))

trait OrdinalDecoder[A] extends Decoder[A]
object OrdinalDecoder {
  private def of[A](name: String, cases: List[A]): OrdinalDecoder[A] = new OrdinalDecoder[A] {
    private val caseOf = cases.toVector
    override def apply(c: HCursor): Result[A] = c.as[Int].flatMap { idx =>
      caseOf.lift(idx) match {
        case Some(value) => Right(value)
        case None => Left(DecodingFailure(s"index $idx is out of bounds for enum $name", c.history))
      }
    }
  }

  inline final def derived[A](using mirror: Mirror.SumOf[A]): OrdinalDecoder[A] =
    OrdinalDecoder.of(
      constValue[mirror.MirroredLabel],
      summonSingletonCases[mirror.MirroredElemTypes, A](constValue[mirror.MirroredLabel])
    )
}

trait OrdinalEncoder[A] extends Encoder[A]
object OrdinalEncoder {
  private def of[A](name: String, cases: List[A]): OrdinalEncoder[A] = new OrdinalEncoder[A] {
    override def apply(a: A): Json = {
      cases.indexOf(a).asJson
    }
  }
  inline final def derived[A](using mirror: Mirror.SumOf[A]): OrdinalEncoder[A] =
    OrdinalEncoder.of[A](
      constValue[mirror.MirroredLabel],
      summonSingletonCases[mirror.MirroredElemTypes, A](constValue[mirror.MirroredLabel])
    )
}

enum DogGeneType extends Enum[DogGeneType] derives OrdinalEncoder, OrdinalDecoder  {
  case Standard
  case Super
  case Looped
}

// Scala DogGeneType
enum SDogGeneType {
  case Standard, Super
  case Looped(loopCount: Int)
  
  def strictLength: Boolean = this match
    case Standard | Looped(_) => true
    case _ => false
}



enum DogGeneCategory extends Enum[DogGeneCategory] derives OrdinalEncoder, OrdinalDecoder {
  case Body
  case Legs
  case Head
  case Pattern
  case Misc
}

enum DogGeneSwapCategory extends Enum[DogGeneSwapCategory] derives OrdinalEncoder, OrdinalDecoder {
  case LegSwap
  case BodySwap
  case HeadSwap
  case ColorSwap
}

case class BooleanFromInt(b: Boolean) extends AnyVal

given booleanFromIntCodec: Codec[BooleanFromInt] with {
  override def apply(c: HCursor): Result[BooleanFromInt] = c.as[Int].map(it => BooleanFromInt(it != 0))

  override def apply(a: BooleanFromInt): Json = {
    if (a.b)
      1.asJson
    else {
      0.asJson
    }
  }
}


case class DogGeneTemplate
  ( version: GeneVersion,
    key: String,
    readableName: String,
    length: Int,
    loopCount: Int,
    superMutationValueAddition: Float,
    geneType: DogGeneType,
    geneCategory: DogGeneCategory,
    geneSwapCategory: DogGeneSwapCategory,
    customCurve: Json,
    plusMinus: BooleanFromInt,
    startAtLowestValue: BooleanFromInt,
    applyMinusPropertyToStartingGene: BooleanFromInt,
    discrete: BooleanFromInt,
    dynamicLoopCount: BooleanFromInt
  )
given dogGeneCodec: Codec[DogGeneTemplate] = deriveCodec[DogGeneTemplate]

case class DogGene(value: Float, minValue: Float, maxValue: Float, defaultMaxValue: Float)
