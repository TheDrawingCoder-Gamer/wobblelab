package net.bulbyvr
package wobblelab
package db

import io.circe.syntax.*
import io.circe.*
import io.circe.derivation.*

sealed trait GeneLike:
  def defaultLen: Int
  def geneType: SDogGeneType

sealed abstract class PlusMinusGene(val plus: GeneticProperty, val minus: GeneticProperty) extends GeneLike:
  def defaultLen = plus.defaultLen
  def geneType = plus.geneType


sealed abstract class GeneticProperty(val displayName: String, val rawLen: Int, val geneType: SDogGeneType = SDogGeneType.Standard) extends GeneLike:
  def defaultLen: Int = this.geneType match
    case SDogGeneType.Looped(loopCount) => loopCount * rawLen
    case _ => rawLen
object GeneticProperty {



  case object BodyMetallicPlus extends GeneticProperty("Body Metallic Plus", 5)
  case object BodyMetallicMinus extends GeneticProperty("Body Metallic Minus", 5)
  case object BodyMetallic extends PlusMinusGene(BodyMetallicPlus, BodyMetallicMinus)
  
  case object BodyGlossPlus extends GeneticProperty("Body Gloss Plus", 5)
  case object BodyGlossMinus extends GeneticProperty("Body Gloss Minus", 5)
  case object BodyGloss extends PlusMinusGene(BodyGlossPlus, BodyGlossMinus)

  case object BodyEmissionColorRPlus extends GeneticProperty("Body Emission Red Plus", 5)
  case object BodyEmissionColorRMinus extends GeneticProperty("Body Emission Red Minus", 5)
  case object BodyEmissionColorR extends PlusMinusGene(BodyEmissionColorRPlus, BodyEmissionColorRMinus)
  
  case object BodyEmissionColorGPlus extends GeneticProperty("Body Emission Green Plus", 5)
  case object BodyEmissionColorGMinus extends GeneticProperty("Body Emission Green Minus", 5)
  case object BodyEmissionColorG extends PlusMinusGene(BodyEmissionColorGPlus, BodyEmissionColorGMinus)
  
  case object BodyEmissionColorBPlus extends GeneticProperty("Body Emission Blue Plus", 5)
  case object BodyEmissionColorBMinus extends GeneticProperty("Body Emission Blue Minus", 5)
  case object BodyEmissionColorB extends PlusMinusGene(BodyEmissionColorBPlus, BodyEmissionColorBMinus)
  
  case object BodyColorRPlus extends GeneticProperty("Body Color Red Plus", 5)
  case object BodyColorRMinus extends GeneticProperty("Body Color Red Minus", 5)
  case object BodyColorR extends PlusMinusGene(BodyColorRPlus, BodyColorRMinus)
  
  case object BodyColorGPlus extends GeneticProperty("Body Color Green Plus", 5)
  case object BodyColorGMinus extends GeneticProperty("Body Color Green Minus", 5)
  case object BodyColorG extends PlusMinusGene(BodyColorGPlus, BodyColorGMinus)
  
  case object BodyColorBPlus extends GeneticProperty("Body Color Blue Plus", 5)
  case object BodyColorBMinus extends GeneticProperty("Body Color Blue Minus", 5)
  case object BodyColorB extends PlusMinusGene(BodyColorBPlus, BodyColorBMinus)
  
  case object LegMetallicPlus extends GeneticProperty("Leg Metallic Plus", 5)
  case object LegMetallicMinus extends GeneticProperty("Leg Metallic Minus", 5)
  case object LegMetallic extends PlusMinusGene(LegMetallicPlus, LegMetallicMinus)
  
  case object LegGlossPlus extends GeneticProperty("Leg Gloss Plus", 5)
  case object LegGlossMinus extends GeneticProperty("Leg Gloss Minus", 5)
  case object LegGloss extends PlusMinusGene(LegGlossPlus, LegGlossMinus)
  
  case object LegEmissionColorRPlus extends GeneticProperty("Leg Emission Red Plus", 5)
  case object LegEmissionColorRMinus extends GeneticProperty("Leg Emission Red Minus", 5)
  case object LegEmissionColorR extends PlusMinusGene(LegEmissionColorRPlus, LegEmissionColorRMinus)
  
  case object LegEmissionColorGPlus extends GeneticProperty("Leg Emission Green Plus", 5)
  case object LegEmissionColorGMinus extends GeneticProperty("Leg Emission Green Minus", 5)
  case object LegEmissionColorG extends PlusMinusGene(LegEmissionColorGPlus, LegEmissionColorGMinus)
  
  case object LegEmissionColorBPlus extends GeneticProperty("Leg Emission Blue Plus", 5)
  case object LegEmissionColorBMinus extends GeneticProperty("Leg Emission Blue Minus", 5)
  case object LegEmissionColorB extends PlusMinusGene(LegEmissionColorBPlus, LegEmissionColorBMinus)
  
  case object LegColorRPlus extends GeneticProperty("Leg Color Red Plus", 5)
  case object LegColorRMinus extends GeneticProperty("Leg Color Red Minus", 5)
  case object LegColorR extends PlusMinusGene(LegColorRPlus, LegColorRMinus)
  
  case object LegColorGPlus extends GeneticProperty("Leg Color Green Plus", 5)
  case object LegColorGMinus extends GeneticProperty("Leg Color Green Minus", 5)
  case object LegColorG extends PlusMinusGene(LegColorGPlus, LegColorGMinus)
  
  case object LegColorBPlus extends GeneticProperty("Leg Color Blue Plus", 5)
  case object LegColorBMinus extends GeneticProperty("Leg Color Blue Minus", 5)
  case object LegColorB extends PlusMinusGene(LegColorBPlus, LegColorBMinus)
  
  case object NoseEarMetallicPlus extends GeneticProperty("Nose/Ear Metallic Plus", 5)
  case object NoseEarMetallicMinus extends GeneticProperty("Nose/Ear Metallic Minus", 5)
  case object NoseEarMetallic extends PlusMinusGene(NoseEarMetallicPlus, NoseEarMetallicMinus)
  
  case object NoseEarGlossPlus extends GeneticProperty("Nose/Ear Gloss Plus", 5)
  case object NoseEarGlossMinus extends GeneticProperty("Nose/Ear Gloss Minus", 5)
  case object NoseEarGloss extends PlusMinusGene(NoseEarGlossPlus, NoseEarGlossMinus)
  
  case object NoseEarEmissionColorRPlus extends GeneticProperty("Nose/Ear Emission Red Plus", 5)
  case object NoseEarEmissionColorRMinus extends GeneticProperty("Nose/Ear Emission Red Minus", 5)
  case object NoseEarEmissionColorR extends PlusMinusGene(NoseEarEmissionColorRPlus, NoseEarEmissionColorRMinus)
  
  case object NoseEarEmissionColorGPlus extends GeneticProperty("Nose/Ear Emission Green Plus", 5)
  case object NoseEarEmissionColorGMinus extends GeneticProperty("Nose/Ear Emission Green Minus", 5)
  case object NoseEarEmissionColorG extends PlusMinusGene(NoseEarEmissionColorGPlus, NoseEarEmissionColorGMinus)
  
  case object NoseEarEmissionColorBPlus extends GeneticProperty("Nose/Ear Emission Blue Plus", 5)
  case object NoseEarEmissionColorBMinus extends GeneticProperty("Nose/Ear Emission Blue Minus", 5)
  case object NoseEarEmissionColorB extends PlusMinusGene(NoseEarEmissionColorBPlus, NoseEarEmissionColorBMinus)
  
  case object NoseEarColorRPlus extends GeneticProperty("Nose/Ear Color Red Plus", 5)
  case object NoseEarColorRMinus extends GeneticProperty("Nose/Ear Color Red Minus", 5)
  case object NoseEarColorR extends PlusMinusGene(NoseEarColorRPlus, NoseEarColorRMinus)
  
  case object NoseEarColorGPlus extends GeneticProperty("Nose/Ear Color Green Plus", 5)
  case object NoseEarColorGMinus extends GeneticProperty("Nose/Ear Color Green Minus", 5)
  case object NoseEarColorG extends PlusMinusGene(NoseEarColorGPlus, NoseEarColorGMinus)
  
  case object NoseEarColorBPlus extends GeneticProperty("Nose/Ear Color Blue Plus", 5)
  case object NoseEarColorBMinus extends GeneticProperty("Nose/Ear Color Blue Minus", 5)
  case object NoseEarColorB extends PlusMinusGene(NoseEarColorBPlus, NoseEarColorBMinus)
  
  case object NoseModAPlus extends GeneticProperty("Nose Size Plus", 5)
  case object NoseModAMinus extends GeneticProperty("Nose Size Minus", 5)
  case object NoseModA extends PlusMinusGene(NoseModAPlus, NoseModAMinus)
  
  case object HornSizePlus extends GeneticProperty("Horn Size Plus", 5)
  case object HornSizeMinus extends GeneticProperty("Horn Size Minus", 5)
  case object HornSize extends PlusMinusGene(HornSizePlus, HornSizeMinus)
  
  case object EarModAPlus extends GeneticProperty("Ear Length Plus", 5)
  case object EarModAMinus extends GeneticProperty("Ear Length Minus", 5)
  case object EarModA extends PlusMinusGene(EarModAPlus, EarModAMinus)
  
  case object EarCurlLeft extends GeneticProperty("Ear Curl Left", 5)
  case object EarCurlRight extends GeneticProperty("Ear Curl Right", 5)
  
  case object SnoutModAPlus extends GeneticProperty("Snout Rotation Plus", 5)
  case object SnoutModAMinus extends GeneticProperty("Snout Rotation Minus", 5)
  case object SnoutModA extends PlusMinusGene(SnoutModAPlus, SnoutModAMinus)
  
  case object SnoutModBPlus extends GeneticProperty("Snout Length Plus", 5)
  case object SnoutModBMinus extends GeneticProperty("Snout Length Minus", 5)
  case object SnoutModB extends PlusMinusGene(SnoutModBPlus, SnoutModBMinus)
  
  case object SnoutModCPlus extends GeneticProperty("Snout Size Plus", 5)
  case object SnoutModCMinus extends GeneticProperty("Snout Size Minus", 5)
  case object SnoutModC extends PlusMinusGene(SnoutModCPlus, SnoutModCMinus)
  
  case object HeadSizePlus extends GeneticProperty("Head Size Plus", 5)
  case object HeadSizeMinus extends GeneticProperty("Head Size Minus", 5)
  case object HeadSize extends PlusMinusGene(HeadSizePlus, HeadSizeMinus)
  
  case object HeadNumber extends GeneticProperty("Head Number", 5, SDogGeneType.Super)
  
  case object BodyScaleXPlus extends GeneticProperty("Body Length Plus", 5, SDogGeneType.Super)
  case object BodyScaleXMinus extends GeneticProperty("Body Length Minus", 5, SDogGeneType.Super)
  case object BodyScaleX extends PlusMinusGene(BodyScaleXPlus, BodyScaleXMinus)
  
  case object BodyScaleZPlus extends GeneticProperty("Body Height Plus", 5, SDogGeneType.Super)
  case object BodyScaleZMinus extends GeneticProperty("Body Height Minus", 5, SDogGeneType.Super)
  case object BodyScaleZ extends PlusMinusGene(BodyScaleZPlus, BodyScaleZMinus)
  
  case object BodyScaleYPlus extends GeneticProperty("Body Width Plus", 5, SDogGeneType.Super)
  case object BodyScaleYMinus extends GeneticProperty("Body Width Minus", 5, SDogGeneType.Super)
  case object BodyScaleY extends PlusMinusGene(BodyScaleYPlus, BodyScaleYMinus)
  
  case object BodyScaleYZPlus extends GeneticProperty("Body Girth Plus", 5, SDogGeneType.Super)
  case object BodyScaleYZMinus extends GeneticProperty("Body Girth Minus", 5, SDogGeneType.Super)
  case object BodyScaleYZ extends PlusMinusGene(BodyScaleYZPlus, BodyScaleYZMinus)
  
  case object BodyScaleGlobalPlus extends GeneticProperty("Body Size Plus", 5, SDogGeneType.Super)
  case object BodyScaleGlobalMinus extends GeneticProperty("Body Size Minus", 5, SDogGeneType.Super)
  case object BodyScaleGlobal extends PlusMinusGene(BodyScaleGlobalPlus, BodyScaleGlobalMinus)
  
  case object TailScalePlus extends GeneticProperty("Tail Size Plus", 5, SDogGeneType.Super)
  case object TailScaleMinus extends GeneticProperty("Tail Size Minus", 5, SDogGeneType.Super)
  case object TailScale extends PlusMinusGene(TailScalePlus, TailScaleMinus)
  
  case object TailNum extends GeneticProperty("Tail Number", 5, SDogGeneType.Super)
  
  case object WingSizePlus extends GeneticProperty("Wing Size Plus", 5)
  case object WingSizeMinus extends GeneticProperty("Wing Size Minus", 5)
  case object WingSize extends PlusMinusGene(WingSizePlus, WingSizeMinus)
  
  case object WingNumber extends GeneticProperty("Wing Number", 10, SDogGeneType.Super)
  
  case object LegScaleXZFrontPlus extends GeneticProperty("Front Leg Girth Plus",5, SDogGeneType.Super)
  case object LegScaleXZFrontMinus extends GeneticProperty("Front Leg Girth Minus", 5, SDogGeneType.Super)
  case object LegScaleXZFront extends PlusMinusGene(LegScaleXZFrontPlus, LegScaleXZFrontMinus)
  
  case object LegScaleXZBackPlus extends GeneticProperty("Back Leg Girth Plus", 5, SDogGeneType.Super)
  case object LegScaleXZBackMinus extends GeneticProperty("Back Leg Girth Minus", 5, SDogGeneType.Super)
  case object LegScaleXZBack extends PlusMinusGene(LegScaleXZBackPlus, LegScaleXZBackMinus)
  
  case object LegScaleYFrontTopPlus extends GeneticProperty("Front Top Leg Length Plus", 5, SDogGeneType.Super)
  case object LegScaleYFrontTopMinus extends GeneticProperty("Front Top Leg Length Minus", 5, SDogGeneType.Super)
  case object LegScaleYFrontTop extends PlusMinusGene(LegScaleYFrontTopPlus, LegScaleYFrontTopMinus)
  
  case object LegScaleYFrontBotPlus extends GeneticProperty("Front Bottom Leg Length Plus", 5, SDogGeneType.Super)
  case object LegScaleYFrontBotMinus extends GeneticProperty("Front Bottom Leg Length Minus", 5, SDogGeneType.Super)
  case object LegScaleYFrontBot extends PlusMinusGene(LegScaleYFrontBotPlus, LegScaleYFrontBotMinus)
  
  case object LegScaleYBackTopPlus extends GeneticProperty("Back Top Leg Length Plus", 5, SDogGeneType.Super)
  case object LegScaleYBackTopMinus extends GeneticProperty("Back Top Leg Length Minus", 5, SDogGeneType.Super)
  case object LegScaleYBackTop extends PlusMinusGene(LegScaleYBackTopPlus, LegScaleYBackTopMinus)
  
  case object LegScaleYBackBotPlus extends GeneticProperty("Back Bottom Leg Length Plus", 5, SDogGeneType.Super)
  case object LegScaleYBackBotMinus extends GeneticProperty("Back Bottom Leg Length Minus", 5, SDogGeneType.Super)
  case object LegScaleYBackBot extends PlusMinusGene(LegScaleYBackBotPlus, LegScaleYBackBotMinus)
  
  case object StanceWidthFrontPlus extends GeneticProperty("Front Leg Stance Width Plus", 5)
  case object StanceWidthFrontMinus extends GeneticProperty("Front Leg Stance Width Minus", 5)
  case object StanceWidthFront extends PlusMinusGene(StanceWidthFrontPlus, StanceWidthFrontMinus)
  
  case object StanceWidthBackPlus extends GeneticProperty("Back Leg Stance Width Plus", 5)
  case object StanceWidthBackMinus extends GeneticProperty("Back Leg Stance Width Minus", 5)
  case object StanceWidthBack extends PlusMinusGene(StanceWidthBackPlus, StanceWidthBackMinus)
  
  case object LegPairsFront extends GeneticProperty("Front Leg Number", 15, SDogGeneType.Super)
  case object LegPairsBack extends GeneticProperty("Back Leg Number", 15, SDogGeneType.Super)
  
  case object PatternColorRPlus extends GeneticProperty("Pattern Color Red Plus", 5)
  case object PatternColorRMinus extends GeneticProperty("Pattern Color Red Minus", 5)
  case object PatternColorR extends PlusMinusGene(PatternColorRPlus, PatternColorRMinus)
  
  case object PatternColorGPlus extends GeneticProperty("Pattern Color Green Plus", 5)
  case object PatternColorGMinus extends GeneticProperty("Pattern Color Green Minus", 5)
  case object PatternColorG extends PlusMinusGene(PatternColorGPlus, PatternColorGMinus)
  
  case object PatternColorBPlus extends GeneticProperty("Pattern Color Blue Plus", 5)
  case object PatternColorBMinus extends GeneticProperty("Pattern Color Blue Minus", 5)
  case object PatternColorB extends PlusMinusGene(PatternColorBPlus, PatternColorBMinus)
  
  case object PatternEmissionColorRPlus extends GeneticProperty("Pattern Emission Red Plus", 5)
  case object PatternEmissionColorRMinus extends GeneticProperty("Pattern Emission Red Minus", 5)
  case object PatternEmissionColorR extends PlusMinusGene(PatternEmissionColorRPlus, PatternEmissionColorRMinus)
  
  case object PatternEmissionColorGPlus extends GeneticProperty("Pattern Emission Green Plus", 5)
  case object PatternEmissionColorGMinus extends GeneticProperty("Pattern Emission Green Minus", 5)
  case object PatternEmissionColorG extends PlusMinusGene(PatternEmissionColorGPlus, PatternEmissionColorGMinus)
  
  case object PatternEmissionColorBPlus extends GeneticProperty("Pattern Emission Blue Plus", 5)
  case object PatternEmissionColorBMinus extends GeneticProperty("Pattern Emission Blue Minus", 5)
  case object PatternEmissionColorB extends PlusMinusGene(PatternEmissionColorBPlus, PatternEmissionColorBMinus)
  
  case object PatternAlpha extends GeneticProperty("Pattern Intensity", 5)
  case object PatternMetallicPlus extends GeneticProperty("Pattern Metallic Plus", 5)
  case object PatternMetallicMinus extends GeneticProperty("Pattern Metallic Minus", 5)
  case object PatternMetallic extends PlusMinusGene(PatternMetallicPlus, PatternMetallicMinus)
  case object PatternSmoothnessPlus extends GeneticProperty("Pattern Smoothness Plus", 5)
  case object PatternSmoothnessMinus extends GeneticProperty("Pattern Smoothness Minus", 5)
  case object PatternSmoothness extends PlusMinusGene(PatternSmoothnessPlus, PatternSmoothnessMinus)
  case object PatternNum extends GeneticProperty("Pattern Frequency", 5)
  case object PatternFlipX extends GeneticProperty("Pattern Horz. Flip", 1, SDogGeneType.Looped(25))
  case object PatternFlipY extends GeneticProperty("Pattern Vert. Flip", 1, SDogGeneType.Looped(25))
  case object PatternInfo extends GeneticProperty("Pattern Variation", 5, SDogGeneType.Looped(25))
  
  
  val values: Map[String, GeneticProperty] = util.Macros.summonSingletonMap[GeneticProperty]
}

// move stupid objects to delay inlining
object PlusMinusGene:
  val values: Map[String, PlusMinusGene] = util.Macros.summonSingletonMap[PlusMinusGene]


given geneticPropertyCodec: Codec[GeneticProperty] = ConfiguredEnumCodec.derived[GeneticProperty](using conf = Configuration.default)