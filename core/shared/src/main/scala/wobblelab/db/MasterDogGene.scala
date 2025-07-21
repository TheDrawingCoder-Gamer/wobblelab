package net.bulbyvr
package wobblelab
package db



import net.bulbyvr.wobblelab.db.MasterDogGene.{minusString, randomSeedSize, separatorSymbol}
import net.bulbyvr.wobblelab.util.ColorF
import net.bulbyvr.wobblelab.DogMaterialPart

import scala.collection.mutable as mut

case class RawGene(
                  dogGene: String,
                  domRecGene: String
                  )

case class DogContext(age: DogAge)

case class MasterDogGene
  ( randomSeed: String,
    geneticHolders: Map[GeneticProperty, DogGeneHolder],
    domRecGenes: List[DomRecGene],
    domRecPropertyStatus: Map[DomRecGeneProperty, Boolean]
  ) {
  def getGeneString(key: GeneticProperty): Option[String] = {
    geneticHolders.get(key).map {
      case DogGeneHolder.Standard(geneString) => geneString
      case DogGeneHolder.Super(geneString, _, _) => geneString
      case l : DogGeneHolder.Looped => l.rawGene
    }
  }

  def updatedGeneString(key: GeneticProperty, value: String): Option[MasterDogGene] = {
    val isValid = !key.geneType.strictLength || key.defaultLen == value.length
    Option.when(isValid):
      val newHolders = geneticHolders.updatedWith(key) {
        case Some(holder) => {
          Some(
            holder match {
              case _: DogGeneHolder.Standard=> DogGeneHolder.Standard(value)
              case e: DogGeneHolder.Super => e.copy(geneString = value)
              case l: DogGeneHolder.Looped => l.copy(rawGene = value)
            }
          )
        }
        case None => None

      }
      copy(
        geneticHolders = newHolders
      )
  }

  def getDynamicSeparatedFloatFromGene(key: GeneticProperty, minVal: Float, maxVal: Float): Option[Float] = {
    getGeneString(key).flatMap { geneString =>
      DogMath.getDynamicFloatFromSequence(key, geneString, minVal, maxVal)
    }
  }

  def getDynamicSeparatedIntFromGene(key: GeneticProperty, minVal: Float, maxVal: Float): Option[Int] = {
    getGeneString(key).flatMap { geneString =>
      DogMath.getDynamicIntFromSequence(key, geneString, minVal, maxVal)
    }
  }

  def inferFloatFromGene(key: GeneticProperty & HasDefiniteBounds): Float = {
    val r = getFloatFromGene(key, key.minBound, key.maxBound)
    key.cap match
      case Some(v) => math.min(r, v)
      case _ => r
  }

  def getFloatFromGene(key: GeneticProperty, minVal: Float, maxVal: Float): Float = {
    geneticHolders(key) match {
      case DogGeneHolder.Super(s, _, _) => DogMath.getDynamicFloatFromSequence(key, s, minVal, maxVal).get
      case DogGeneHolder.Standard(s) => DogMath.getFloatFromGeneSequence(s, minVal, maxVal)
      case DogGeneHolder.Looped(s, _, _) => DogMath.getFloatFromGeneSequence(s, minVal, maxVal)
    }
  }

  def getIntFromGene(key: GeneticProperty, minVal: Float, maxVal: Float): Int = {
    geneticHolders(key) match {
      // special casing for Super, it clamps strings shorter than normal length to the min val
      // .get - assert that yes we know this is super
      case DogGeneHolder.Super(s, _, _) => DogMath.getDynamicIntFromSequence(key, s, minVal, maxVal).get
      case _ => Math.round(getFloatFromGene(key, minVal, maxVal))
    }
  }

  def inferIntFromGene(key: GeneticProperty & HasDefiniteBounds): Int =
    val r = getIntFromGene(key, key.minBound, key.maxBound)
    key.cap match
      case Some(v) => math.min(r, v.toInt)
      case None => r

  def getMaxValIncrease(key: GeneticProperty): Float = {
    geneticHolders.get(key).flatMap {
      case s: DogGeneHolder.Super => Some(s.maxValIncrease)
      case _ => None
    }.getOrElse(0f)
  }
  
  def updateDomRec(idx: Int, kind: TraitType): MasterDogGene =
    val old = this.domRecGenes(idx)
    val daNew = old.copy(value = kind)
    val newList = this.domRecGenes.updated(idx, daNew)
    val newMap = this.domRecPropertyStatus.updated(old.currentProperty, false).updated(daNew.currentProperty, true)
    this.copy(domRecGenes = newList, domRecPropertyStatus = newMap)

  def inferUpdatePlusMinus(prop: PlusMinusGene & HasDefiniteBounds, value: Float): MasterDogGene = updatePlusMinus(prop, value, prop.minBound, prop.maxBound)

  def updatePlusMinus(prop: PlusMinusGene, value: Float, minVal: Float, maxVal: Float): MasterDogGene =
    val n = value - minVal
    val minusGene = if n < 0 then DogMath.maybeDynamicFloatToGeneSequence(math.abs(n), 0, minVal, prop.defaultLen, !prop.geneType.strictLength) else "0".repeat(prop.defaultLen)
    val plusGene = if n < 0 then "0".repeat(prop.defaultLen) else DogMath.maybeDynamicFloatToGeneSequence(math.abs(n), 0, maxVal, prop.defaultLen, !prop.geneType.strictLength)
    val newMap = geneticHolders.updatedWith[DogGeneHolder](prop.plus) {
      case Some(x@DogGeneHolder.Super(_,_,_)) => Some(x.copy(geneString = plusGene))
      case Some(DogGeneHolder.Standard(_)) => Some(DogGeneHolder.Standard(plusGene))
      case Some(_) => ???
      case _ => None
    }.updatedWith(prop.minus) {
      case Some(x@DogGeneHolder.Super(_,_,_)) => Some(x.copy(geneString = minusGene))
      case Some(DogGeneHolder.Standard(_)) => Some(DogGeneHolder.Standard(minusGene))
      case Some(_) => ???
      case _ => None
    }

    this.copy(geneticHolders = newMap)

  // If Prop is a super, then value is _unclamped_. Otherwise, value is clamped to range
  def updateFloatValue(prop: GeneticProperty, value: Float, minVal: Float, maxVal: Float): MasterDogGene =
    val newMap = geneticHolders.updatedWith(prop) {
      case Some(x@DogGeneHolder.Super(_,_,_)) => Some(x.copy(geneString = DogMath.dynamicFloatToGeneSequence(value, minVal, maxVal, prop.defaultLen)))
      case Some(DogGeneHolder.Standard(_)) => Some(DogGeneHolder.Standard(DogMath.floatToGeneSequence(value, minVal, maxVal, prop.defaultLen)))
      case Some(x@DogGeneHolder.Looped(_, _, _)) => Some(x.copy(rawGene = DogMath.floatToGeneSequence(value, minVal, maxVal, prop.defaultLen)))
      case _ => None
    }
    copy(geneticHolders = newMap)

  def inferUpdateFloatValue(prop: GeneticProperty & HasDefiniteBounds, value: Float): MasterDogGene =
    updateFloatValue(prop, value, prop.minBound, prop.maxBound)

  def updatePartMaterial(name: DogMaterialPart, mat: CalculatedMaterial): MasterDogGene =
    import DogMaterialPart.*
    val default = name match
      case Body => Dog.defaultMaterials.body
      case Leg => Dog.defaultMaterials.legs
      case NoseEar => Dog.defaultMaterials.earsNose

    import default.*

    val propMap = this.geneticHolders.to(mut.HashMap)

    val daName = name.toString

    def addPlusOrMinus(prop: PlusMinusGene, minVal: Float, maxVal: Float, v: Float): Unit =
      val n = v - minVal
      if n < 0 then
        val minusGene = DogMath.floatToGeneSequence(math.abs(n), 0, minVal, prop.defaultLen)
        propMap.update(prop.plus, DogGeneHolder.Standard("0".repeat(prop.defaultLen)))
        propMap.update(prop.minus, DogGeneHolder.Standard(minusGene))
      else

        val plusGene = DogMath.floatToGeneSequence(n, 0, maxVal, prop.defaultLen)
        propMap.update(prop.plus, DogGeneHolder.Standard(plusGene))
        propMap.update(prop.minus, DogGeneHolder.Standard("0".repeat(prop.defaultLen)))


    addPlusOrMinus(PlusMinusGene.valueOf(daName + "Metallic"), minMetallic, maxMetallic, mat.metallic)
    addPlusOrMinus(PlusMinusGene.valueOf(daName + "Gloss"), minGlossiness, maxGlossiness, mat.glossiness)
    addPlusOrMinus(PlusMinusGene.valueOf(daName + "ColorR"), minBaseR, maxBaseR, mat.base.r)
    addPlusOrMinus(PlusMinusGene.valueOf(daName + "ColorG"), minBaseG, maxBaseG, mat.base.g)
    addPlusOrMinus(PlusMinusGene.valueOf(daName + "ColorB"), minBaseB, maxBaseB, mat.base.b)
    addPlusOrMinus(PlusMinusGene.valueOf(daName + "EmissionColorR"), minEmissionR, maxEmissionR, mat.emission.r)
    addPlusOrMinus(PlusMinusGene.valueOf(daName + "EmissionColorG"), minEmissionG, maxEmissionG, mat.emission.g)
    addPlusOrMinus(PlusMinusGene.valueOf(daName + "EmissionColorB"), minEmissionB, maxEmissionB, mat.emission.b)

    copy(geneticHolders = propMap.toMap)


  def getPlusMinus(prop: PlusMinusGene, minVal: Float, maxVal: Float): Float = {
    val plusValue = getFloatFromGene(prop.plus, 0, maxVal)
    // minusValue is
    val minusValue = getFloatFromGene(prop.minus, 0, minVal)
    plusValue - minusValue
  }

  def inferPlusMinus(prop: PlusMinusGene & HasDefiniteBounds): Float =
    val r = getPlusMinus(prop, prop.minBound, prop.maxBound)
    prop.cap match
      case Some(c) => math.min(r, c)
      case None => r

  def getValue(prop: Gene, minVal: Float, maxVal: Float): Float =
    prop match
      case x: PlusMinusGene => getPlusMinus(x, minVal, maxVal)
      case x: GeneticProperty => getFloatFromGene(x, minVal, maxVal)

  // minVal and maxVal's meanings are different depending on plusMinus vs. normal
  def getPercent(prop: Gene, minVal: Float, maxVal: Float): Float =
    prop match
      case x: PlusMinusGene =>
        // minVal is how far _left_ on the number line we can go,
        // maxVal is how far _right_
        val res = getPlusMinus(x, minVal, maxVal)
        val percent = (res + minVal) / (maxVal + minVal)
        percent
      case x: GeneticProperty =>
        // minVal and maxVal are bounds
        val res = getFloatFromGene(x, minVal, maxVal)
        val percent = (res - minVal) / (maxVal - minVal)
        percent

  def inferPercent(prop: Gene & HasDefiniteBounds): Float =
    getPercent(prop, prop.minBound, prop.maxBound)

  def inferValue(prop: Gene & HasDefiniteBounds): Float =
    prop match
      case x: (PlusMinusGene & HasDefiniteBounds) => inferPlusMinus(x)
      case x: (GeneticProperty & HasDefiniteBounds) => inferFloatFromGene(x)

  private def calculatePlusMinus(name: String, minVal: Float, maxVal: Float): Float =
    getPlusMinus(PlusMinusGene.valueOf(name), minVal, maxVal)


  def getPartMaterial(part: DogMaterialPart): CalculatedMaterial =
    val name = part.toString
    val default = part match
      case DogMaterialPart.Body => Dog.defaultMaterials.body
      case DogMaterialPart.Leg => Dog.defaultMaterials.legs
      case DogMaterialPart.NoseEar => Dog.defaultMaterials.earsNose
    import default.*
    val metallic = minMetallic + calculatePlusMinus(name + "Metallic", minMetallic, maxMetallic)
    val glossiness = minGlossiness + calculatePlusMinus(name + "Gloss", minGlossiness, maxGlossiness)
    val baseR = minBaseR + calculatePlusMinus(name + "ColorR", minBaseR, maxBaseR)
    val baseG = minBaseG + calculatePlusMinus(name + "ColorG", minBaseG, maxBaseG)
    val baseB = minBaseB + calculatePlusMinus(name + "ColorB", minBaseB, maxBaseB)
    val base = ColorF(baseR, baseG, baseB)
    val emiR = minEmissionR + calculatePlusMinus(name + "EmissionColorR", minEmissionR, maxEmissionR)
    val emiG = minEmissionG + calculatePlusMinus(name + "EmissionColorG", minEmissionG, maxEmissionG)
    val emiB = minEmissionB + calculatePlusMinus(name + "EmissionColorB", minEmissionB, maxEmissionB)
    val emission = ColorF(emiR, emiG, emiB)
    CalculatedMaterial(base, emission, metallic, glossiness)


  def earType: EarType =
    val earSharp = this.domRecPropertyStatus(DomRecGeneProperty.EarSharp)
    val earConic = this.domRecPropertyStatus(DomRecGeneProperty.EarConic)
    val earFilled = this.domRecPropertyStatus(DomRecGeneProperty.EarFilled)
    val earFloppy = this.domRecPropertyStatus(DomRecGeneProperty.EarFloppy)
    val earHalved = this.domRecPropertyStatus(DomRecGeneProperty.EarHalved)
    val tiltedEars = this.domRecPropertyStatus(DomRecGeneProperty.TiltedEars)
    val earPartialFlop = this.domRecPropertyStatus(DomRecGeneProperty.EarPartialFlop)

    if (earFilled) {
      if (tiltedEars) {
        EarType.TypeB
      } else if (earHalved) {
        EarType.Blunt
      } else {
        EarType.TypeA
      }
    } else if (earSharp && earConic && earHalved)
      EarType.Twisted
    else if (earSharp && earFloppy)
      EarType.Bulbous
    else if (earSharp)
      EarType.Cross
    else if (earFloppy)
      EarType.Bent
    else if (earConic)
      EarType.Horn
    else if (earPartialFlop)
      EarType.Wavy
    else
      EarType.Shepherd

  def boundsFor(prop: Gene)(using dog: DogContext): Option[(Float, Float)] =
    prop match
      case x: AgeBasedBounds => Some((x.minBound, DogMath.ageModifiedValue(x.puppyMax, x.maxBound, dog.age)))
      case x: HasDefiniteBounds => Some((x.minBound, x.maxBound))
      case GeneticProperty.EarModA =>
        val earKind = this.earType
        val bounds = Dog.earModA(earKind)
        Some((bounds.min, bounds.max))

      case _ => None

  def calculateGenes()(using dog: DogContext): CalculatedGenes = {

    def calculateStandard(key: GeneticProperty, minVal: Float, maxVal: Float, isSuper: Boolean = true): Float = {
      val value = if (isSuper) getDynamicSeparatedFloatFromGene(key, minVal, maxVal).get else getFloatFromGene(key, minVal, maxVal)
      value
    }
    val wingType = {
      val noWings = this.domRecPropertyStatus(DomRecGeneProperty.NoWings)
      val alignmentGood = this.domRecPropertyStatus(DomRecGeneProperty.AlignmentGood)
      val alignmentEvil = this.domRecPropertyStatus(DomRecGeneProperty.AlignmentEvil)
      val wingFeathers = this.domRecPropertyStatus(DomRecGeneProperty.WingFeathers)
      val wingIssues = this.domRecPropertyStatus(DomRecGeneProperty.WingIssues)
      val alignmentNeutral = this.domRecPropertyStatus(DomRecGeneProperty.AlignmentNeutral)
      val missingLeftWing = wingIssues && this.domRecPropertyStatus(DomRecGeneProperty.MissingLeftWing)
      val missingRightWing = wingIssues && this.domRecPropertyStatus(DomRecGeneProperty.MissingRightWing)

      if (noWings || (missingLeftWing && missingRightWing)) {
        WingType.NoWings
      } else if (!wingFeathers) {
        if (alignmentEvil)
          WingType.Bat
        else
          WingType.Vestigial
      } else if (alignmentNeutral)
        WingType.Paradise
      else if (alignmentGood)
        WingType.Angel
      else if (alignmentEvil)
        WingType.Vulture
      else
        WingType.NoWings
    }
    val noseType = {
      val noseFlat = this.domRecPropertyStatus(DomRecGeneProperty.NoseFlat)
      val noseSquish = this.domRecPropertyStatus(DomRecGeneProperty.NoseSquish)
      val noseStretch = this.domRecPropertyStatus(DomRecGeneProperty.NoseStretch)
      val noseRepeated = this.domRecPropertyStatus(DomRecGeneProperty.NoseRepeated)
      val noseExtrusion = this.domRecPropertyStatus(DomRecGeneProperty.NoseExtrusion)

      if (noseExtrusion && noseRepeated && noseSquish)
        NoseType.Wide
      else if (noseRepeated && noseSquish)
        NoseType.Square
      else if (noseRepeated && noseExtrusion)
        NoseType.Pug
      else if (noseRepeated && noseFlat)
        NoseType.Mallow
      else if (noseFlat && noseStretch)
        NoseType.Triangle
      else if (noseStretch)
        NoseType.Greyhound
      else if (noseExtrusion)
        NoseType.Bulb
      else if (noseFlat)
        NoseType.HalfMallow
      else
        NoseType.TypeA
    }

    val earType = this.earType
    val hornType = {
      val hornsCenter = this.domRecPropertyStatus(DomRecGeneProperty.HornsCenter)
      val hornsTraditional = this.domRecPropertyStatus(DomRecGeneProperty.HornsTraditional)
      val noHorns = this.domRecPropertyStatus(DomRecGeneProperty.HornsNone)
      val hornsCurled = this.domRecPropertyStatus(DomRecGeneProperty.HornsCurled)
      val hornsNub = this.domRecPropertyStatus(DomRecGeneProperty.HornsNub)
      val hornsThick = this.domRecPropertyStatus(DomRecGeneProperty.HornsThick)
      val hornsThin = this.domRecPropertyStatus(DomRecGeneProperty.HornsThin)
      if (noHorns || (!hornsCenter && !hornsTraditional))
        HornType.NoHorns
      else if (hornsNub)
        HornType.Nub
      else if (hornsThick)
        HornType.Thick
      else if (hornsThin)
        HornType.Thin
      else if (hornsCurled)
        HornType.Curled
      else
        HornType.NoHorns
    }
    val hornPlacement = {
      val centerHorn = this.domRecPropertyStatus(DomRecGeneProperty.HornsCenter)
      val traditionalHorns = this.domRecPropertyStatus(DomRecGeneProperty.HornsTraditional)
      if (centerHorn)
        HornPlacement.HornPlacementCenter
      else if (traditionalHorns)
        HornPlacement.HornPlacementTraditional
      else
        HornPlacement.HornPlacementNone
    }
    val tailType = {
      val noTail = this.domRecPropertyStatus(DomRecGeneProperty.NoTail)
      val thinTail = this.domRecPropertyStatus(DomRecGeneProperty.ThinTail)
      val nubTail = this.domRecPropertyStatus(DomRecGeneProperty.NubTail)
      val flatTail = this.domRecPropertyStatus(DomRecGeneProperty.FlatTail)
      val stiffTail = this.domRecPropertyStatus(DomRecGeneProperty.StiffTail)
      val bulbousTail = this.domRecPropertyStatus(DomRecGeneProperty.BulbousTail)
      val tail3D = this.domRecPropertyStatus(DomRecGeneProperty.Tail3D)
      val repeatedTail = this.domRecPropertyStatus(DomRecGeneProperty.RepeatedTail)
      val curledTail = this.domRecPropertyStatus(DomRecGeneProperty.CurledTail)
      val slightlyCurledTail = this.domRecPropertyStatus(DomRecGeneProperty.SlightlyCurledTail)
      if (noTail)
        TailType.NoTail
      else if (!tail3D) {
        if (stiffTail) {
          if (slightlyCurledTail)
            TailType.StiffSlightlyCurly
          else if (curledTail)
            TailType.StiffCurly
          else
            TailType.Stiff
        } else if (nubTail)
          TailType.Nub
        else
          TailType.Flowy
      } else if (nubTail && slightlyCurledTail)
        TailType.Lifted
      else if (thinTail && stiffTail)
        TailType.Whip
      else if (repeatedTail && slightlyCurledTail)
        TailType.Curl
      else if (repeatedTail && curledTail)
        TailType.DoubleCurl
      else if (repeatedTail && flatTail)
        TailType.Feral
      else if (bulbousTail && flatTail)
        TailType.Tri
      else if (bulbousTail)
        TailType.Bulbous
      else if (flatTail)
        TailType.Paddle
      else
        TailType.Plume
    }
    val headNumber = this.inferIntFromGene(GeneticProperty.HeadNumber)
    val tailNumber =
      this.getDynamicSeparatedIntFromGene(GeneticProperty.TailNum, Dog.tailNumMin, Dog.tailNumMax).get
    var frontLegPairs =
      math.max(1,
        math.floor(this.inferFloatFromGene(GeneticProperty.LegPairsFront) / Dog.legNumberIncreaseRate).toInt
      )
    var backLegPairs =
      math.max(1,
        math.floor(this.inferFloatFromGene(GeneticProperty.LegPairsBack) / Dog.legNumberIncreaseRate).toInt
      )
    
    if (backLegPairs + frontLegPairs > Dog.legNumberHardCap) {
      var num2 = 1
      while (backLegPairs + frontLegPairs > Dog.legNumberHardCap) {
        if (num2 % 2 == 0) {
          if (backLegPairs <= 1) {
            frontLegPairs -= 1
          } else {
            backLegPairs -= 1
          }
        } else if (frontLegPairs <= 1) {
          backLegPairs -= 1
        } else {
          frontLegPairs -= 1
        }
        num2 += 1
      }
    }
    if (frontLegPairs < 1)
      frontLegPairs = 1
    if (backLegPairs < 1)
      backLegPairs = 1
      
    val wingNumber =
      this.getDynamicSeparatedIntFromGene(GeneticProperty.WingNumber, Dog.wingNumberMin, Dog.wingNumberMax).get

    val eyeType = {
      val eyelids = this.domRecPropertyStatus(DomRecGeneProperty.Eyelids)
      val oblongEyes = this.domRecPropertyStatus(DomRecGeneProperty.OblongEyes)
      val smallPupils = this.domRecPropertyStatus(DomRecGeneProperty.SmallPupils)
      val multiPupils = this.domRecPropertyStatus(DomRecGeneProperty.MultiPupils)
      val geometricEyes = this.domRecPropertyStatus(DomRecGeneProperty.GeometricEyes)
      val decorativeEyes = this.domRecPropertyStatus(DomRecGeneProperty.DecorativeEyes)
      val lashesEyes = this.domRecPropertyStatus(DomRecGeneProperty.LashesEyes)
      val longEyes = this.domRecPropertyStatus(DomRecGeneProperty.LongEyes)
      val missingPupilEyes = this.domRecPropertyStatus(DomRecGeneProperty.MissingPupilEyes)
      val horizontalEyes = this.domRecPropertyStatus(DomRecGeneProperty.HorizontalEyes)
      val spiralEyes = this.domRecPropertyStatus(DomRecGeneProperty.SpiralEyes)
      val triangleEyes = this.domRecPropertyStatus(DomRecGeneProperty.TriangleEyes)
      if (oblongEyes && multiPupils) {
        EyeType.Double
      } else if (smallPupils && multiPupils) {
        EyeType.Spider
      } else if (geometricEyes && spiralEyes) {
        EyeType.Hex
      } else if (horizontalEyes && longEyes) {
        EyeType.Slim
      } else if (horizontalEyes && oblongEyes)
        EyeType.Puck
      else if (missingPupilEyes && multiPupils)
        EyeType.Mitosis
      else if (eyelids)
        EyeType.Lidded
      else if (oblongEyes)
        EyeType.Oblong
      else if (smallPupils)
        EyeType.Concerned
      else if (triangleEyes)
        EyeType.Triangle
      else if (geometricEyes)
        EyeType.Square
      else if (lashesEyes)
        EyeType.Lashes
      else if (spiralEyes)
        EyeType.Spiral
      else if (missingPupilEyes)
        EyeType.Pupilless
      else if (decorativeEyes)
        EyeType.Keyhole
      else
        EyeType.Standard
    }

    val mouthType = {
      val teeth = this.domRecPropertyStatus(DomRecGeneProperty.Teeth)
      val vMouth = this.domRecPropertyStatus(DomRecGeneProperty.VMouth)
      val mouthSmile = this.domRecPropertyStatus(DomRecGeneProperty.MouthSmile)
      val mouthFrown = this.domRecPropertyStatus(DomRecGeneProperty.MouthFrown)
      val mouthCheeks = this.domRecPropertyStatus(DomRecGeneProperty.MouthCheeks)
      val mouthCutoff = this.domRecPropertyStatus(DomRecGeneProperty.MouthCutoff)
      val mouthWiggle = this.domRecPropertyStatus(DomRecGeneProperty.MouthWiggle)
      val openMouth = this.domRecPropertyStatus(DomRecGeneProperty.OpenMouth)
      val mouthPointed = this.domRecPropertyStatus(DomRecGeneProperty.MouthPointed)
      val mouthNeutral = this.domRecPropertyStatus(DomRecGeneProperty.MouthNeutral)
      val mouthMissingTeeth = this.domRecPropertyStatus(DomRecGeneProperty.MouthMissingTeeth)

      if (teeth && mouthNeutral)
        MouthType.Standard
      else if (vMouth && openMouth)
        MouthType.Ah
      else if (teeth && mouthPointed)
        MouthType.Diamond
      else if (teeth && mouthCutoff)
        MouthType.Wise
      else if (teeth && mouthWiggle)
        MouthType.Wobbly
      else if (teeth && mouthFrown)
        MouthType.Boom
      else if (teeth && mouthSmile)
        MouthType.Smug
      else if (mouthMissingTeeth && mouthPointed)
        MouthType.Toothy
      else if (mouthMissingTeeth)
        MouthType.MouthBreather
      else if (mouthNeutral && !openMouth)
        MouthType.Blank
      else if (mouthCheeks)
        MouthType.Cheeky
      else if (vMouth)
        MouthType.Simple
      else if (mouthFrown)
        MouthType.Pointed
      else
        MouthType.MouthNone
    }

    val floatMap = mut.Map[Gene, CalculatedValue]()

    Gene.floatValues.foreach:
      case _: DontSave => ()
      case x =>
        this.boundsFor(x) match
          case Some(v) =>
            floatMap(x) = CalculatedValue(getValue(x, v._1, v._2), getPercent(x, v._1, v._2))
          case _ => ()
    

    
    val bodyMat = getPartMaterial(DogMaterialPart.Body)
    val legMat = getPartMaterial(DogMaterialPart.Leg)
    val noseEarMat = getPartMaterial(DogMaterialPart.NoseEar)

    CalculatedGenes(
      bodyMat = bodyMat,
      legColor = legMat,
      noseEarColor = noseEarMat,
      floatItems = floatMap.toMap,
      headNumber = headNumber,
      tailNumber = tailNumber,
      wingNumber = wingNumber,
      frontLegPairs = frontLegPairs,
      backLegPairs = backLegPairs,
      earType = earType,
      hornType = hornType,
      hornPlacement = hornPlacement,
      noseType = noseType,
      tailType = tailType,
      wingType = wingType,
      eyeType = eyeType,
      mouthType = mouthType
    )
  }


  def getRawString: RawGene = {
    val stringBuilder = new mut.StringBuilder()
    stringBuilder.append(randomSeed)
    for (gene <- GenesDb.genesDb.dogGenes) {
      if (gene.geneType != DogGeneType.Super) {
        val key = gene.key
        if (gene.plusMinus.b) {
          val key2 = key + MasterDogGene.plusString
          val key3 = key + MasterDogGene.minusString
          stringBuilder.append(getGeneString(GeneticProperty.valueOf(key2)).get)
          stringBuilder.append(getGeneString(GeneticProperty.valueOf(key3)).get)
        } else {
          stringBuilder.append(getGeneString(GeneticProperty.valueOf(key)).get)
        }
      }
    }
    GenesDb.genesDb.dogGenes.foreach { gene =>
      if (gene.geneType == DogGeneType.Super) {
        stringBuilder.append(separatorSymbol)
        val key = gene.key
        if (gene.plusMinus.b) {
          val plusKey = gene.key + MasterDogGene.plusString
          val minusKey = gene.key + MasterDogGene.minusString
          stringBuilder.append(getGeneString(GeneticProperty.valueOf(plusKey)).get)
          stringBuilder.append(separatorSymbol)
          stringBuilder.append(getGeneString(GeneticProperty.valueOf(minusKey)).get)
        } else {
          stringBuilder.append(getGeneString(GeneticProperty.valueOf(key)).get)
        }
      }
    }

    val dogGene = stringBuilder.toString
    val domRecBuilder = new mut.StringBuilder()

    domRecGenes.foreach { domRec =>
      domRecBuilder.append(domRec.value.geneRepr)
    }

    RawGene(dogGene, domRecBuilder.toString)

  }
}

object MasterDogGene {
  val randomSeedSize = 10
  val separatorSymbol = "|"
  val plusString = "Plus"
  val minusString = "Minus"

  def fromGenes(inDogGene: String, domRecGene: String): MasterDogGene = {
    var dogGene = inDogGene
    val randomSeed = dogGene.substring(0, randomSeedSize)
    var currentSuperIndex = 0
    var currentStandardIndex = randomSeedSize
    // val geneValues = mut.Map[GeneticProperty, DogGene]()
    val geneticHolders = mut.Map[GeneticProperty, DogGeneHolder]()
    val domRecPropertyStatus = mut.Map[DomRecGeneProperty, Boolean]()
    val domRecGenes = domRecGene.grouped(2).zip(GenesDb.genesDb.domRecGenes).map { (it, domRecGene) =>
      val ttype
        = if (it(0) != it(1))
          then TraitType.Het
          else if it(0) == 'A'
          then TraitType.Dom
          else TraitType.Sub
      domRecGene(ttype)
    }.toList
    DomRecGeneProperty.values.foreach { it =>
      domRecPropertyStatus(it) = false
    }
    domRecGenes.foreach { domRecGene =>
      domRecPropertyStatus(domRecGene.currentProperty) = true
    }

    def mapSuperGene(gene: DogGeneTemplate, optionalKeyAddition: String = "", minus: Boolean = false): Unit = {
      var num = dogGene.indexOf(separatorSymbol, currentSuperIndex) + 1
      if (num == 0) {
        // WHAT THE FREAK
        println("what the skibidi...")
        dogGene += generateBaseGeneOfSize(gene.length)
        num = dogGene.indexOf(separatorSymbol, currentSuperIndex) + 1
      }
      var num2 = dogGene.indexOf(separatorSymbol, num)
      if (num2 == -1) {
        num2 = dogGene.length
      }
      val freakyProperty = GeneticProperty.valueOf(gene.key + optionalKeyAddition)
      var newMaxValIncrease = gene.superMutationValueAddition
      // never actually used???????????????
      if (minus) {
        newMaxValIncrease = 0f
      }
      geneticHolders(freakyProperty) = DogGeneHolder.Super(dogGene.substring(num, num2), gene.length, newMaxValIncrease)
      currentSuperIndex = num
    }
    def mapStandardGene(gene: DogGeneTemplate, optionalKeyAddition: String = ""): Unit = {
      val freakyProperty = GeneticProperty.valueOf(gene.key + optionalKeyAddition)
      geneticHolders(freakyProperty) = DogGeneHolder.Standard(dogGene.substring(currentStandardIndex, currentStandardIndex + gene.length))
      currentStandardIndex += gene.length
    }
    def mapLoopedGene(gene: DogGeneTemplate, optionalKeyAddition: String = ""): Unit = {
      var num = gene.loopCount
      if (gene.dynamicLoopCount.b) {
        // base game never calls this (yet)
        ???
      }
      // may break if gene is old? i eepy
      val value = DogGeneHolder.Looped(dogGene.substring(currentStandardIndex, currentStandardIndex + gene.length * num), gene.length, gene.discrete.b)

      val freakyProperty = GeneticProperty.valueOf(gene.key + optionalKeyAddition)
      geneticHolders(freakyProperty) = value
      currentStandardIndex += gene.length * num
    }
    for (gene <- GenesDb.genesDb.dogGenes) {
        gene.geneType match {
          case DogGeneType.Standard => {
            if (gene.plusMinus.b) {
              mapStandardGene(gene, plusString)
              mapStandardGene(gene, minusString)
            } else {
              mapStandardGene(gene)
            }
          }
          case DogGeneType.Super => {
            if (gene.plusMinus.b) {
              mapSuperGene(gene, plusString)
              // base game doesnt use minus arg???
              mapSuperGene(gene, minusString)
            } else {
              mapSuperGene(gene)
            }
          }
          case DogGeneType.Looped => {
            if (gene.plusMinus.b) {
              // this isn't used in the base game yet
              mapLoopedGene(gene, plusString)
              mapLoopedGene(gene, minusString)
            } else {
              mapLoopedGene(gene)
            }
          }
        }
    }
    MasterDogGene(dogGene.substring(0, randomSeedSize), geneticHolders.toMap, domRecGenes, domRecPropertyStatus.toMap)
  }

  private def generateBaseGeneOfSize(size: Int, addSeperator: Boolean = true): String = {
    (if (addSeperator) separatorSymbol else "") + "0".repeat(size)
  }

}
