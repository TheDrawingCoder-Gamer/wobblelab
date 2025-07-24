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

// Scala DogGeneType
enum SDogGeneType {
  case Standard
  case Super(maxValIncrease: Float)
  case Looped(loopCount: Int, discrete: Boolean = true)
  
  def strictLength: Boolean = this match
    case Standard | Looped(_, _) => true
    case _ => false
}



enum DogGeneCategory extends Enum[DogGeneCategory] {
  case Body
  case Legs
  case Head
  case Pattern
  case Misc
}

enum DogGeneSwapCategory extends Enum[DogGeneSwapCategory] {
  case LegSwap
  case BodySwap
  case HeadSwap
  case ColorSwap
}
