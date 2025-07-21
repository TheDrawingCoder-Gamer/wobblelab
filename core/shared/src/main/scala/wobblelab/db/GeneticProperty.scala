package net.bulbyvr
package wobblelab
package db

import io.circe.syntax.*
import io.circe.*
import io.circe.derivation.*

sealed trait GeneLike:
  def parent: Gene
  def defaultLen: Int
  def geneType: SDogGeneType
  def displayName: String

trait HasDefiniteBounds:
  def minBound: Float
  def maxBound: Float
  def cap: Option[Float] = None

trait AgeBasedBounds extends HasDefiniteBounds:
  def puppyMax: Float

// Gene is something that would make sense to display to an outside person as a standalone value
// so, not plus minus LOLLLLL
sealed trait Gene:
  def displayName: String


sealed abstract class PlusMinusGene(val displayName: String, val plus: GeneticProperty, val minus: GeneticProperty, val rawLen: Int, val geneType: SDogGeneType = SDogGeneType.Standard, val usesMinusArg: Boolean = false) extends GeneLike, Gene:
  def parent: Gene = this
  
  def defaultLen: Int = this.geneType match
    case SDogGeneType.Looped(loopCount) => loopCount * rawLen
    case _ => rawLen 

trait DontSave

// What's actually _in the genome_ is a genetic property
sealed trait GeneticProperty extends GeneLike

sealed abstract class PlainGeneticProperty(val displayName: String, val rawLen: Int, val geneType: SDogGeneType = SDogGeneType.Standard) extends Gene, GeneticProperty:
  def defaultLen: Int = this.geneType match
    case SDogGeneType.Looped(loopCount) => loopCount * rawLen
    case _ => rawLen
  
  def parent: Gene = this
  

sealed abstract class PlusGeneticProperty(myParent: => PlusMinusGene) extends GeneticProperty, GeneLike:
  lazy val parent: PlusMinusGene = myParent
  // def parent: Gene = myParent
  lazy val displayName: String = parent.displayName + " Plus"

  override def defaultLen: Int = parent.defaultLen

  override def geneType: SDogGeneType = myParent.geneType
  
sealed abstract class MinusGeneticProperty(myParent: => PlusMinusGene) extends GeneticProperty, GeneLike:
  lazy val parent: PlusMinusGene = myParent
  lazy val displayName: String = parent.displayName + " Minus"

  override def defaultLen: Int = parent.defaultLen

  override def geneType: SDogGeneType = parent.geneType

object GeneticProperty {

  import Dog.defaultMaterials.*

  case object BodyMetallicPlus extends PlusGeneticProperty(BodyMetallic)
  case object BodyMetallicMinus extends MinusGeneticProperty(BodyMetallic)
  case object BodyMetallic extends PlusMinusGene("Body Metallic", BodyMetallicPlus, BodyMetallicMinus, 5), HasDefiniteBounds, DontSave:
    def minBound: Float = body.minMetallic
    def maxBound: Float = body.maxMetallic
  
  case object BodyGlossPlus extends PlusGeneticProperty(BodyGloss)
  case object BodyGlossMinus extends MinusGeneticProperty(BodyGloss)
  case object BodyGloss extends PlusMinusGene("Body Gloss", BodyGlossPlus, BodyGlossMinus, 5), HasDefiniteBounds, DontSave:
    def minBound: Float = body.minGlossiness
    def maxBound: Float = body.maxGlossiness


  case object BodyEmissionColorRPlus extends PlusGeneticProperty(BodyEmissionColorR)
  case object BodyEmissionColorRMinus extends MinusGeneticProperty(BodyEmissionColorR)
  case object BodyEmissionColorR extends PlusMinusGene("Body Emission Red", BodyEmissionColorRPlus, BodyEmissionColorRMinus, 5), HasDefiniteBounds, DontSave:
    def minBound: Float = body.minEmissionR
    def maxBound: Float = body.maxEmissionR
  
  case object BodyEmissionColorGPlus extends PlusGeneticProperty(BodyEmissionColorG)
  case object BodyEmissionColorGMinus extends MinusGeneticProperty(BodyEmissionColorG)
  case object BodyEmissionColorG extends PlusMinusGene("Body Emission Green",BodyEmissionColorGPlus, BodyEmissionColorGMinus,  5), HasDefiniteBounds, DontSave:
    def minBound: Float = body.minEmissionG
    def maxBound: Float = body.maxEmissionG
  
  case object BodyEmissionColorBPlus extends PlusGeneticProperty(BodyEmissionColorB)
  case object BodyEmissionColorBMinus extends MinusGeneticProperty(BodyEmissionColorB)
  case object BodyEmissionColorB extends PlusMinusGene("Body Emission Blue",BodyEmissionColorBPlus, BodyEmissionColorBMinus,  5), HasDefiniteBounds, DontSave:
    def minBound: Float = body.minEmissionB
    def maxBound: Float = body.maxEmissionB
  
  case object BodyColorRPlus extends PlusGeneticProperty(BodyColorR)
  case object BodyColorRMinus extends MinusGeneticProperty(BodyColorR)
  case object BodyColorR extends PlusMinusGene("Body Color Red",BodyColorRPlus, BodyColorRMinus,  5), HasDefiniteBounds, DontSave:
    def minBound: Float = body.minBaseR
    def maxBound: Float = body.maxBaseR
  
  case object BodyColorGPlus extends PlusGeneticProperty(BodyColorG)
  case object BodyColorGMinus extends MinusGeneticProperty(BodyColorG)
  case object BodyColorG extends PlusMinusGene("Body Color Green",BodyColorGPlus, BodyColorGMinus,  5), HasDefiniteBounds, DontSave:
    def minBound: Float = body.minBaseG
    def maxBound: Float = body.maxBaseG
  
  case object BodyColorBPlus extends PlusGeneticProperty(BodyColorB)
  case object BodyColorBMinus extends MinusGeneticProperty(BodyColorB)
  case object BodyColorB extends PlusMinusGene("Body Color Blue",BodyColorBPlus, BodyColorBMinus,  5), HasDefiniteBounds, DontSave:
    def minBound: Float = body.minBaseB
    def maxBound: Float = body.maxBaseB

  
  case object LegMetallicPlus extends PlusGeneticProperty(LegMetallic)
  case object LegMetallicMinus extends MinusGeneticProperty(LegMetallic)
  case object LegMetallic extends PlusMinusGene("Leg Metallic",LegMetallicPlus, LegMetallicMinus,  5), HasDefiniteBounds, DontSave:
    def minBound: Float = legs.minMetallic
    def maxBound: Float = legs.maxMetallic

  
  case object LegGlossPlus extends PlusGeneticProperty(LegGloss)
  case object LegGlossMinus extends MinusGeneticProperty(LegGloss)
  case object LegGloss extends PlusMinusGene("Leg Gloss",LegGlossPlus, LegGlossMinus,  5), HasDefiniteBounds, DontSave:
    def minBound: Float = legs.minGlossiness
    def maxBound: Float = legs.maxGlossiness
  
  case object LegEmissionColorRPlus extends PlusGeneticProperty(LegEmissionColorR)
  case object LegEmissionColorRMinus extends MinusGeneticProperty(LegEmissionColorR)
  case object LegEmissionColorR extends PlusMinusGene("Leg Emission Red",LegEmissionColorRPlus, LegEmissionColorRMinus,  5), HasDefiniteBounds, DontSave:
    def minBound: Float = legs.minEmissionR
    def maxBound: Float = legs.maxEmissionR
  
  case object LegEmissionColorGPlus extends PlusGeneticProperty(LegEmissionColorG)
  case object LegEmissionColorGMinus extends MinusGeneticProperty(LegEmissionColorG)
  case object LegEmissionColorG extends PlusMinusGene("Leg Emission Green",LegEmissionColorGPlus, LegEmissionColorGMinus, 5), HasDefiniteBounds, DontSave:
    def minBound: Float = legs.minEmissionG
    def maxBound: Float = legs.maxEmissionG
  
  case object LegEmissionColorBPlus extends PlusGeneticProperty(LegEmissionColorB)
  case object LegEmissionColorBMinus extends MinusGeneticProperty(LegEmissionColorB)
  case object LegEmissionColorB extends PlusMinusGene("Leg Emission Blue",LegEmissionColorBPlus, LegEmissionColorBMinus,  5), HasDefiniteBounds, DontSave:
    def minBound: Float = legs.minEmissionB
    def maxBound: Float = legs.maxEmissionB
  
  case object LegColorRPlus extends PlusGeneticProperty(LegColorR)
  case object LegColorRMinus extends MinusGeneticProperty(LegColorR)
  case object LegColorR extends PlusMinusGene("Leg Color Red",LegColorRPlus, LegColorRMinus,  5), HasDefiniteBounds, DontSave:
    def minBound: Float = legs.minBaseR
    def maxBound: Float = legs.maxBaseR
  
  case object LegColorGPlus extends PlusGeneticProperty(LegColorG)
  case object LegColorGMinus extends MinusGeneticProperty(LegColorG)
  case object LegColorG extends PlusMinusGene("Leg Color Green",LegColorGPlus, LegColorGMinus,  5), HasDefiniteBounds, DontSave:
    def minBound: Float = legs.minBaseG
    def maxBound: Float = legs.maxBaseG
  
  case object LegColorBPlus extends PlusGeneticProperty(LegColorB)
  case object LegColorBMinus extends MinusGeneticProperty(LegColorB)
  case object LegColorB extends PlusMinusGene("Leg Color Blue",LegColorBPlus, LegColorBMinus,  5), HasDefiniteBounds, DontSave:
    def minBound: Float = legs.minBaseB
    def maxBound: Float = legs.maxBaseB
  
  case object NoseEarMetallicPlus extends PlusGeneticProperty(NoseEarMetallic)
  case object NoseEarMetallicMinus extends MinusGeneticProperty(NoseEarMetallic)
  case object NoseEarMetallic extends PlusMinusGene("Nose/Ear Metallic",NoseEarMetallicPlus, NoseEarMetallicMinus,  5), HasDefiniteBounds, DontSave:
    def minBound: Float = earsNose.minMetallic
    def maxBound: Float = earsNose.maxMetallic
  
  case object NoseEarGlossPlus extends PlusGeneticProperty(NoseEarGloss)
  case object NoseEarGlossMinus extends MinusGeneticProperty(NoseEarGloss)
  case object NoseEarGloss extends PlusMinusGene("Nose/Ear Gloss",NoseEarGlossPlus, NoseEarGlossMinus,  5), HasDefiniteBounds, DontSave:
    def minBound: Float = earsNose.minGlossiness
    def maxBound: Float = earsNose.maxGlossiness
  
  case object NoseEarEmissionColorRPlus extends PlusGeneticProperty(NoseEarEmissionColorR)
  case object NoseEarEmissionColorRMinus extends MinusGeneticProperty(NoseEarEmissionColorR)
  case object NoseEarEmissionColorR extends PlusMinusGene("Nose/Ear Emission Red",NoseEarEmissionColorRPlus, NoseEarEmissionColorRMinus,  5), HasDefiniteBounds, DontSave:
    def minBound: Float = earsNose.minEmissionR
    def maxBound: Float = earsNose.maxEmissionR
  
  case object NoseEarEmissionColorGPlus extends PlusGeneticProperty(NoseEarEmissionColorG)
  case object NoseEarEmissionColorGMinus extends MinusGeneticProperty(NoseEarEmissionColorG)
  case object NoseEarEmissionColorG extends PlusMinusGene("Nose/Ear Emission Green",NoseEarEmissionColorGPlus, NoseEarEmissionColorGMinus,  5), HasDefiniteBounds, DontSave:
    def minBound: Float = earsNose.minEmissionG
    def maxBound: Float = earsNose.maxEmissionG
  
  case object NoseEarEmissionColorBPlus extends PlusGeneticProperty(NoseEarEmissionColorB)
  case object NoseEarEmissionColorBMinus extends MinusGeneticProperty(NoseEarEmissionColorB)
  case object NoseEarEmissionColorB extends PlusMinusGene("Nose/Ear Emission Blue",NoseEarEmissionColorBPlus, NoseEarEmissionColorBMinus,  5), HasDefiniteBounds, DontSave:
    def minBound: Float = earsNose.minEmissionB
    def maxBound: Float = earsNose.maxEmissionB
  
  case object NoseEarColorRPlus extends PlusGeneticProperty(NoseEarColorR)
  case object NoseEarColorRMinus extends MinusGeneticProperty(NoseEarColorR)
  case object NoseEarColorR extends PlusMinusGene("Nose/Ear Color Red",NoseEarColorRPlus, NoseEarColorRMinus,  5), HasDefiniteBounds, DontSave:
    def minBound: Float = earsNose.minBaseR
    def maxBound: Float = earsNose.maxBaseR
  
  case object NoseEarColorGPlus extends PlusGeneticProperty(NoseEarColorG)
  case object NoseEarColorGMinus extends MinusGeneticProperty(NoseEarColorG)
  case object NoseEarColorG extends PlusMinusGene("Nose/Ear Color Green",NoseEarColorGPlus, NoseEarColorGMinus,  5), HasDefiniteBounds, DontSave:
    def minBound: Float = earsNose.minBaseG
    def maxBound: Float = earsNose.maxBaseG
  
  case object NoseEarColorBPlus extends PlusGeneticProperty(NoseEarColorB)
  case object NoseEarColorBMinus extends MinusGeneticProperty(NoseEarColorB)
  case object NoseEarColorB extends PlusMinusGene("Nose/Ear Color Blue",NoseEarColorBPlus, NoseEarColorBMinus,  5), HasDefiniteBounds, DontSave:
    def minBound: Float = earsNose.minBaseB
    def maxBound: Float = earsNose.maxBaseB
  
  case object NoseModAPlus extends PlusGeneticProperty(NoseModA)
  case object NoseModAMinus extends MinusGeneticProperty(NoseModA)
  case object NoseModA extends PlusMinusGene("Nose Size",NoseModAPlus, NoseModAMinus,  5)
  
  case object HornSizePlus extends PlusGeneticProperty(HornSize)
  case object HornSizeMinus extends MinusGeneticProperty(HornSize)
  case object HornSize extends PlusMinusGene("Horn Size",HornSizePlus, HornSizeMinus,  5), HasDefiniteBounds:
    override def minBound: Float = Dog.hornSizeMin
    override def maxBound: Float = Dog.hornSizeMax


  case object EarModAPlus extends PlusGeneticProperty(EarModA)
  case object EarModAMinus extends MinusGeneticProperty(EarModA)
  case object EarModA extends PlusMinusGene("Ear Length",EarModAPlus, EarModAMinus,  5)
  
  case object EarCurlLeft extends PlainGeneticProperty("Ear Curl Left", 5)
  case object EarCurlRight extends PlainGeneticProperty("Ear Curl Right", 5)
  
  case object SnoutModAPlus extends PlusGeneticProperty(SnoutModA)
  case object SnoutModAMinus extends MinusGeneticProperty(SnoutModA)
  case object SnoutModA extends PlusMinusGene("Snout Rotation",SnoutModAPlus, SnoutModAMinus,  5), HasDefiniteBounds:
    def minBound: Float = Dog.snoutModRotYMin
    def maxBound: Float = Dog.snoutModRotYMax
  
  case object SnoutModBPlus extends PlusGeneticProperty(SnoutModB)
  case object SnoutModBMinus extends MinusGeneticProperty(SnoutModB)
  case object SnoutModB extends PlusMinusGene("Snout Length",SnoutModBPlus, SnoutModBMinus,  5), HasDefiniteBounds:
    def minBound: Float = Dog.snoutModLenMin
    def maxBound: Float = Dog.snoutModLenMax
  
  case object SnoutModCPlus extends PlusGeneticProperty(SnoutModC)
  case object SnoutModCMinus extends MinusGeneticProperty(SnoutModC)
  case object SnoutModC extends PlusMinusGene("Snout Size",SnoutModCPlus, SnoutModCMinus,  5), HasDefiniteBounds:
    def minBound: Float = Dog.snoutModScaleMin
    def maxBound: Float = Dog.snoutModScaleMax
  
  case object HeadSizePlus extends PlusGeneticProperty(HeadSize)
  case object HeadSizeMinus extends MinusGeneticProperty(HeadSize)
  case object HeadSize extends PlusMinusGene("Head Size",HeadSizePlus, HeadSizeMinus,  5), HasDefiniteBounds:
    def minBound: Float = Dog.headSizeMin
    def maxBound: Float = Dog.headSizeMax
    override def cap: Option[Float] = Some(Dog.headSizeCap)
  
  case object HeadNumber extends PlainGeneticProperty("Head Number", 5, SDogGeneType.Super(1)), HasDefiniteBounds, DontSave:
    def minBound: Float = Dog.headNumMin
    def maxBound: Float = Dog.headNumMax
    override def cap: Option[Float] = Some(Dog.headCap)
  
  case object BodyScaleXPlus extends PlusGeneticProperty(BodyScaleX)
  case object BodyScaleXMinus extends MinusGeneticProperty(BodyScaleX)
  case object BodyScaleX extends PlusMinusGene("Body Length",BodyScaleXPlus, BodyScaleXMinus,  5, SDogGeneType.Super(0.1), usesMinusArg = true), HasDefiniteBounds:
    def minBound: Float = Dog.bodyScaleXMin
    def maxBound: Float = Dog.bodyScaleXMax
    override def cap: Option[Float] = Some(Dog.bodyScaleXCap)
  
  case object BodyScaleZPlus extends PlusGeneticProperty(BodyScaleZ)
  case object BodyScaleZMinus extends MinusGeneticProperty(BodyScaleZ)
  case object BodyScaleZ extends PlusMinusGene("Body Height",BodyScaleZPlus, BodyScaleZMinus,  5, SDogGeneType.Super(0.1), usesMinusArg = true), HasDefiniteBounds:
    def minBound: Float = Dog.bodyScaleZMin
    def maxBound: Float = Dog.bodyScaleZMax
    override def cap: Option[Float] = Some(Dog.bodyScaleZCap)
  
  case object BodyScaleYPlus extends PlusGeneticProperty(BodyScaleY)
  case object BodyScaleYMinus extends MinusGeneticProperty(BodyScaleY)
  case object BodyScaleY extends PlusMinusGene("Body Width",BodyScaleYPlus, BodyScaleYMinus,  5, SDogGeneType.Super(0.1), usesMinusArg = true), HasDefiniteBounds:
    def minBound: Float = Dog.bodyScaleYMin

    def maxBound: Float = Dog.bodyScaleYMax

    override def cap: Option[Float] = Some(Dog.bodyScaleYCap)
  
  case object BodyScaleYZPlus extends PlusGeneticProperty(BodyScaleYZ)
  case object BodyScaleYZMinus extends MinusGeneticProperty(BodyScaleYZ)
  case object BodyScaleYZ extends PlusMinusGene("Body Girth",BodyScaleYZPlus, BodyScaleYZMinus,  5, SDogGeneType.Super(0.1), usesMinusArg = true), HasDefiniteBounds:
    def minBound: Float = Dog.bodyScaleYZMin
    def maxBound: Float = Dog.bodyScaleYZMax
    override def cap: Option[Float] = Some(Dog.bodyScaleYZCap)
  
  case object BodyScaleGlobalPlus extends PlusGeneticProperty(BodyScaleGlobal)
  case object BodyScaleGlobalMinus extends MinusGeneticProperty(BodyScaleGlobal)
  case object BodyScaleGlobal extends PlusMinusGene("Body Size",BodyScaleGlobalPlus, BodyScaleGlobalMinus,  5, SDogGeneType.Super(0.05), usesMinusArg = true), HasDefiniteBounds:
    def minBound: Float = Dog.dogScaleGlobalMin
    def maxBound: Float = Dog.dogScaleGlobalMax
    override def cap: Option[Float] = Some(Dog.dogScaleGlobalCap)
  
  case object TailScalePlus extends PlusGeneticProperty(TailScale)
  case object TailScaleMinus extends MinusGeneticProperty(TailScale)
  case object TailScale extends PlusMinusGene("Tail Size",TailScalePlus, TailScaleMinus,  5, SDogGeneType.Super(0), usesMinusArg = true), HasDefiniteBounds:
    def minBound: Float = Dog.tailScaleMin
    def maxBound: Float = Dog.tailScaleMax
    override def cap: Option[Float] = Some(Dog.tailScaleCap)
  
  case object TailNum extends PlainGeneticProperty("Tail Number", 5, SDogGeneType.Super(1)), HasDefiniteBounds, DontSave:
    def minBound: Float = Dog.tailNumMin
    def maxBound: Float = Dog.tailNumMax
  
  case object WingSizePlus extends PlusGeneticProperty(WingSize)
  case object WingSizeMinus extends MinusGeneticProperty(WingSize)
  // for some reason doesn't use minus arg
  case object WingSize extends PlusMinusGene("Wing Size",WingSizePlus, WingSizeMinus,  5), HasDefiniteBounds:
    def minBound: Float = Dog.wingScaleMin
    def maxBound: Float = Dog.wingScaleMax
  
  case object WingNumber extends PlainGeneticProperty("Wing Number", 10, SDogGeneType.Super(1)), HasDefiniteBounds, DontSave:
    def minBound: Float = Dog.wingNumberMin
    def maxBound: Float = Dog.wingNumberMax
  
  case object LegScaleXZFrontPlus extends PlusGeneticProperty(LegScaleXZFront)
  case object LegScaleXZFrontMinus extends MinusGeneticProperty(LegScaleXZFront)
  case object LegScaleXZFront extends PlusMinusGene("Front Leg Girth",LegScaleXZFrontPlus, LegScaleXZFrontMinus, 5, SDogGeneType.Super(0.1), usesMinusArg = true), AgeBasedBounds:
    def minBound: Float = Dog.legGirthMin
    def maxBound: Float = Dog.legGirthMax

    override def puppyMax: Float = Dog.legGirthMaxPuppy
  
  case object LegScaleXZBackPlus extends PlusGeneticProperty(LegScaleXZBack)
  case object LegScaleXZBackMinus extends MinusGeneticProperty(LegScaleXZBack)
  case object LegScaleXZBack extends PlusMinusGene("Back Leg Girth",LegScaleXZBackPlus, LegScaleXZBackMinus,  5, SDogGeneType.Super(0.1), usesMinusArg = true), AgeBasedBounds:
    def minBound: Float = Dog.legGirthMin
    def maxBound: Float = Dog.legGirthMax
    def puppyMax: Float = Dog.legGirthMaxPuppy
  
  case object LegScaleYFrontTopPlus extends PlusGeneticProperty(LegScaleYFrontTop)
  case object LegScaleYFrontTopMinus extends MinusGeneticProperty(LegScaleYFrontTop)
  case object LegScaleYFrontTop extends PlusMinusGene("Front Top Leg Length",LegScaleYFrontTopPlus, LegScaleYFrontTopMinus,  5, SDogGeneType.Super(0.1), usesMinusArg = true), HasDefiniteBounds:
    def minBound: Float = Dog.legLengthTopMin
    def maxBound: Float = Dog.legLengthTopMax
  
  case object LegScaleYFrontBotPlus extends PlusGeneticProperty(LegScaleYFrontBot)
  case object LegScaleYFrontBotMinus extends MinusGeneticProperty(LegScaleYFrontBot)
  case object LegScaleYFrontBot extends PlusMinusGene("Front Bottom Leg Length",LegScaleYFrontBotPlus, LegScaleYFrontBotMinus,  5, SDogGeneType.Super(0.1), usesMinusArg = true), HasDefiniteBounds:
    def minBound: Float = Dog.legLengthBotMin
    def maxBound: Float = Dog.legLengthBotMax
  
  case object LegScaleYBackTopPlus extends PlusGeneticProperty(LegScaleYBackTop)
  case object LegScaleYBackTopMinus extends MinusGeneticProperty(LegScaleYBackTop)
  case object LegScaleYBackTop extends PlusMinusGene("Back Top Leg Length",LegScaleYBackTopPlus, LegScaleYBackTopMinus,  5, SDogGeneType.Super(0.1), usesMinusArg = true), HasDefiniteBounds:
    def minBound: Float = Dog.legLengthTopMin
    def maxBound: Float = Dog.legLengthTopMax
  
  case object LegScaleYBackBotPlus extends PlusGeneticProperty(LegScaleYBackBot)
  case object LegScaleYBackBotMinus extends MinusGeneticProperty(LegScaleYBackBot)
  case object LegScaleYBackBot extends PlusMinusGene("Back Bottom Leg Length",LegScaleYBackBotPlus, LegScaleYBackBotMinus,  5, SDogGeneType.Super(0.1), usesMinusArg = true), HasDefiniteBounds:
    def minBound: Float = Dog.legLengthBotMin
    def maxBound: Float = Dog.legLengthBotMax
  
  case object StanceWidthFrontPlus extends PlusGeneticProperty(StanceWidthFront)
  case object StanceWidthFrontMinus extends MinusGeneticProperty(StanceWidthFront)
  case object StanceWidthFront extends PlusMinusGene("Front Leg Stance Width",StanceWidthFrontPlus, StanceWidthFrontMinus,  5), HasDefiniteBounds:
    def minBound: Float = Dog.stanceWidthMin
    def maxBound: Float = Dog.stanceWidthMax
  
  case object StanceWidthBackPlus extends PlusGeneticProperty(StanceWidthBack)
  case object StanceWidthBackMinus extends MinusGeneticProperty(StanceWidthBack)
  case object StanceWidthBack extends PlusMinusGene("Back Leg Stance Width",StanceWidthBackPlus, StanceWidthBackMinus,  5), HasDefiniteBounds:
    def minBound: Float = Dog.stanceWidthMin
    def maxBound: Float = Dog.stanceWidthMax
  
  case object LegPairsFront extends PlainGeneticProperty("Front Leg Number", 15, SDogGeneType.Super(1)), HasDefiniteBounds, DontSave:
    def minBound: Float = Dog.legNumberMin
    def maxBound: Float = Dog.legNumberMax
    // No cap, extra calc required
    // override def cap: Option[Float] = Some(Dog.legNumberHardCap)
    
  case object LegPairsBack extends PlainGeneticProperty("Back Leg Number", 15, SDogGeneType.Super(1)), HasDefiniteBounds, DontSave:
    def minBound: Float = Dog.legNumberMin
    def maxBound: Float = Dog.legNumberMax
    // No cap, Extra calc required
    // override def cap: Option[Float] = Some(Dog.legNumberHardCap)
  
  case object PatternColorRPlus extends PlusGeneticProperty(PatternColorR)
  case object PatternColorRMinus extends MinusGeneticProperty(PatternColorR)
  case object PatternColorR extends PlusMinusGene("Pattern Color Red",PatternColorRPlus, PatternColorRMinus,  5), HasDefiniteBounds:
    def minBound: Float = bodyPattern.minBaseR
    def maxBound: Float = bodyPattern.maxBaseR
  
  case object PatternColorGPlus extends PlusGeneticProperty(PatternColorG)
  case object PatternColorGMinus extends MinusGeneticProperty(PatternColorG)
  case object PatternColorG extends PlusMinusGene("Pattern Color Green",PatternColorGPlus, PatternColorGMinus,  5), HasDefiniteBounds:
    def minBound: Float = bodyPattern.minBaseG
    def maxBound: Float = bodyPattern.maxBaseG
  
  case object PatternColorBPlus extends PlusGeneticProperty(PatternColorB)
  case object PatternColorBMinus extends MinusGeneticProperty(PatternColorB)
  case object PatternColorB extends PlusMinusGene("Pattern Color Blue",PatternColorBPlus, PatternColorBMinus,  5), HasDefiniteBounds:
    def minBound: Float = bodyPattern.minBaseB
    def maxBound: Float = bodyPattern.maxBaseB
  
  case object PatternEmissionColorRPlus extends PlusGeneticProperty(PatternEmissionColorR)
  case object PatternEmissionColorRMinus extends MinusGeneticProperty(PatternEmissionColorR)
  case object PatternEmissionColorR extends PlusMinusGene("Pattern Emission Red",PatternEmissionColorRPlus, PatternEmissionColorRMinus,  5), HasDefiniteBounds:
    def minBound: Float = bodyPattern.minEmissionR
    def maxBound: Float = bodyPattern.maxEmissionR
  
  case object PatternEmissionColorGPlus extends PlusGeneticProperty(PatternEmissionColorG)
  case object PatternEmissionColorGMinus extends MinusGeneticProperty(PatternEmissionColorG)
  case object PatternEmissionColorG extends PlusMinusGene("Pattern Emission Green",PatternEmissionColorGPlus, PatternEmissionColorGMinus,  5), HasDefiniteBounds:
    def minBound: Float = bodyPattern.minEmissionG
    def maxBound: Float = bodyPattern.maxEmissionG
  
  case object PatternEmissionColorBPlus extends PlusGeneticProperty(PatternEmissionColorB)
  case object PatternEmissionColorBMinus extends MinusGeneticProperty(PatternEmissionColorB)
  case object PatternEmissionColorB extends PlusMinusGene("Pattern Emission Blue",PatternEmissionColorBPlus, PatternEmissionColorBMinus,  5), HasDefiniteBounds:
    def minBound: Float = bodyPattern.minEmissionB
    def maxBound: Float = bodyPattern.maxEmissionB
  
  case object PatternAlpha extends PlainGeneticProperty("Pattern Intensity", 5), HasDefiniteBounds:
    def minBound: Float = Dog.textureAlphaMin
    def maxBound: Float = Dog.textureAlphaMax

  case object PatternMetallicPlus extends PlusGeneticProperty(PatternMetallic)
  case object PatternMetallicMinus extends MinusGeneticProperty(PatternMetallic)
  case object PatternMetallic extends PlusMinusGene("Pattern Metallic",PatternMetallicPlus, PatternMetallicMinus,  5), HasDefiniteBounds:
    def minBound: Float = bodyPattern.minMetallic
    def maxBound: Float = bodyPattern.maxMetallic

  case object PatternSmoothnessPlus extends PlusGeneticProperty(PatternSmoothness)
  case object PatternSmoothnessMinus extends MinusGeneticProperty(PatternSmoothness)
  case object PatternSmoothness extends PlusMinusGene("Pattern Smoothness",PatternSmoothnessPlus, PatternSmoothnessMinus,  5), HasDefiniteBounds:
    def minBound: Float = bodyPattern.minGlossiness
    def maxBound: Float = bodyPattern.maxGlossiness

  // Pattern frequency not in terms of scale, but in randomness
  // such an odd name
  case object PatternNum extends PlainGeneticProperty("Pattern Frequency", 5), HasDefiniteBounds:
    def minBound: Float = Dog.patternNumMin
    def maxBound: Float = Dog.patternNumMax
  case object PatternFlipX extends PlainGeneticProperty("Pattern Horz. Flip", 1, SDogGeneType.Looped(25))
  case object PatternFlipY extends PlainGeneticProperty("Pattern Vert. Flip", 1, SDogGeneType.Looped(25))
  case object PatternInfo extends PlainGeneticProperty("Pattern Variation", 5, SDogGeneType.Looped(25))


  val valueOf: Map[String, GeneticProperty] = util.Macros.summonSingletonMap[GeneticProperty]

  val values: List[GeneticProperty] = valueOf.values.toList.sortBy(_.displayName)
}

// move stupid objects to delay inlining
object PlusMinusGene:
  val valueOf: Map[String, PlusMinusGene] = util.Macros.summonSingletonMap[PlusMinusGene]
  val values: List[PlusMinusGene] = valueOf.values.toList.sortBy(_.displayName)

object Gene:
  val valueOf: Map[String, Gene] = util.Macros.summonSingletonMap[Gene]
  val floatValues: List[Gene] = List(
    GeneticProperty.NoseModA,
    GeneticProperty.HornSize,
    GeneticProperty.EarModA,
    GeneticProperty.EarCurlLeft,
    GeneticProperty.EarCurlRight,
    GeneticProperty.SnoutModA,
    GeneticProperty.SnoutModB,
    GeneticProperty.SnoutModC,
    GeneticProperty.HeadSize,
    GeneticProperty.BodyScaleX,
    GeneticProperty.BodyScaleY,
    GeneticProperty.BodyScaleZ,
    GeneticProperty.BodyScaleYZ,
    GeneticProperty.BodyScaleGlobal,
    GeneticProperty.TailScale,
    GeneticProperty.WingSize,
    GeneticProperty.LegScaleXZFront,
    GeneticProperty.LegScaleXZBack,
    GeneticProperty.LegScaleYFrontTop,
    GeneticProperty.LegScaleYFrontBot,
    GeneticProperty.LegScaleYBackTop,
    GeneticProperty.LegScaleYBackBot,
    GeneticProperty.StanceWidthFront,
    GeneticProperty.StanceWidthBack
  )
// given geneticPropertyCodec: Codec[GeneticProperty] = ConfiguredEnumCodec.derived[GeneticProperty](using conf = Configuration.default)