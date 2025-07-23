package net.bulbyvr
package wobblelab
package db



import net.bulbyvr.wobblelab.db.MasterDogGene.separatorSymbol
import net.bulbyvr.wobblelab.util.{ColorF, GayMath, Net5Random}
import net.bulbyvr.wobblelab.DogMaterialPart

import scala.collection.mutable as mut
import cats.*
import cats.data.*
import cats.syntax.all.*

case class RawGene(
                  dogGene: String,
                  domRecGene: String
                  )

case class DogContext(age: DogAge)

case class MasterDogGene
  (randomSeed: String,
  // This map WILL contain ALL cases of GeneticProperty
   genes: Map[GeneticProperty, String],
   domRecGenes: List[DomRecGene],
   domRecPropertyStatus: Map[DomRecGeneProperty, Boolean]
  ) {
  def updatedGeneString(key: GeneticProperty, value: String): Option[MasterDogGene] = {
    val isValid =
      (!key.geneType.strictLength || key.defaultLen == value.length)
        && value.forall(it => it == '1' || it == '0')
    Option.when(isValid):
      val newHolders = genes.updated(key, value)
      copy(
        genes = newHolders
      )
  }


  def inferFloatFromGene(key: GeneticProperty & HasDefiniteBounds): Float = {
    val r = getFloatFromGene(key, key.minBound, key.maxBound)
    key.cap match
      case Some(v) => math.min(r, v)
      case _ => r
  }

  def getFloatFromGene(key: GeneticProperty, minVal: Float, maxVal: Float): Float = {
    key.geneType match
      case SDogGeneType.Super(_) => DogMath.getDynamicFloatFromSequence(key, genes(key), minVal, maxVal).get
      case _ => DogMath.getFloatFromGeneSequence(genes(key), minVal, maxVal)
  }

  def getIntFromGene(key: GeneticProperty, minVal: Float, maxVal: Float): Int = {
    key.geneType match
      case SDogGeneType.Super(_) => DogMath.getDynamicIntFromSequence(key, genes(key), minVal, maxVal).get
      case _ => Math.round(getFloatFromGene(key, minVal, maxVal))
  }

  def inferIntFromGene(key: GeneticProperty & HasDefiniteBounds): Int =
    val r = getIntFromGene(key, key.minBound, key.maxBound)
    key.cap match
      case Some(v) => math.min(r, v.toInt)
      case None => r

  def updateDomRec(idx: Int, kind: TraitType): MasterDogGene =
    val old = this.domRecGenes(idx)
    val daNew = old.copy(value = kind)
    val newList = this.domRecGenes.updated(idx, daNew)

    // edge case
    // triangle eyes is the ONLY case (aside from None, but None doesn't matter)
    // where two seperate dom rec genes affect the same property, so we'll have to check the other one
    val oldCase =
      if old.currentProperty == DomRecGeneProperty.TriangleEyes then
        val otherIdx = if idx == 28 then 33 else 28
        val otherProp = this.domRecGenes(otherIdx)
        otherProp.currentProperty == DomRecGeneProperty.TriangleEyes
      else
        false

    val newMap = this.domRecPropertyStatus.updated(old.currentProperty, oldCase).updated(daNew.currentProperty, true)


    this.copy(domRecGenes = newList, domRecPropertyStatus = newMap)

  // TODO: this obviously is broken in some way
  // FIXME!
  def updatePlusMinus(prop: PlusMinusGene, value: Float, minVal: Float, maxVal: Float): ValidatedNec[String, MasterDogGene] =
    println(value)
    val minusGene = if value < 0 then DogMath.maybeDynamicFloatToGeneSequence(prop.minus, math.abs(value), 0, minVal) else Validated.validNec("0".repeat(prop.defaultLen))
    val plusGene = if value < 0 then Validated.validNec("0".repeat(prop.defaultLen)) else DogMath.maybeDynamicFloatToGeneSequence(prop.plus, math.abs(value), 0, maxVal)
    // Validated is applicative (so it can pull all errors out and display them)
    (minusGene, plusGene).mapN: (minus, plus) =>
      this.copy(genes = genes.updated(prop.plus, plus).updated(prop.minus, minus))

  // If Prop is a super, then value is _unclamped_. Otherwise, value is clamped to range
  def updateFloatValue(prop: GeneticProperty, value: Float, minVal: Float, maxVal: Float): ValidatedNec[String, MasterDogGene] =
    val newValue =
      prop.geneType match
        case SDogGeneType.Super(_) => DogMath.dynamicFloatToGeneSequence(prop, value, minVal, maxVal)
        case _ => Validated.validNec(DogMath.floatToGeneSequence(value, minVal, maxVal, prop.defaultLen))

    newValue.map: it =>
      copy(genes = genes.updated(prop, it))
  
  def updatePartBase(name: DogMaterialPart, color: ColorF): MasterDogGene =
    import name.default.*

    // ASSERT IN RANGE
    // Color should ALWAYS be in range
    this
      .updatePlusMinus(name.baseR, color.r - minBaseR, minBaseR, maxBaseR).toOption.get
      .updatePlusMinus(name.baseG, color.g - minBaseG, minBaseG, maxBaseG).toOption.get
      .updatePlusMinus(name.baseB, color.b - minBaseB, minBaseB, maxBaseB).toOption.get
    
  def updatePartEmission(name: DogMaterialPart, color: ColorF): MasterDogGene =
    import name.default.*

    this
      .updatePlusMinus(name.emissionR, color.r - minEmissionR, minEmissionR, maxEmissionR).toOption.get
      .updatePlusMinus(name.emissionG, color.g - minEmissionG, minEmissionG, maxEmissionG).toOption.get
      .updatePlusMinus(name.emissionB, color.b - minEmissionB, minEmissionB, maxEmissionB).toOption.get
    
  
  def updatePartMaterial(name: DogMaterialPart, mat: CalculatedMaterial): MasterDogGene =
    import DogMaterialPart.*
    import name.default.*

    val propMap = this.genes.to(mut.HashMap)

    // this JUST WORKS:tm:
    def addPlusOrMinus(prop: PlusMinusGene, minVal: Float, maxVal: Float, v: Float): Unit =
      val n = v - minVal
      if n < 0 then
        val minusGene = DogMath.floatToGeneSequence(math.abs(n), 0, minVal, prop.defaultLen)
        propMap.update(prop.plus, "0".repeat(prop.defaultLen))
        propMap.update(prop.minus, minusGene)
      else

        val plusGene = DogMath.floatToGeneSequence(n, 0, maxVal, prop.defaultLen)
        propMap.update(prop.plus, plusGene)
        propMap.update(prop.minus, "0".repeat(prop.defaultLen))


    addPlusOrMinus(name.metallic, minMetallic, maxMetallic, mat.metallic)
    addPlusOrMinus(name.glossiness, minGlossiness, maxGlossiness, mat.glossiness)
    addPlusOrMinus(name.baseR, minBaseR, maxBaseR, mat.base.r)
    addPlusOrMinus(name.baseG, minBaseG, maxBaseG, mat.base.g)
    addPlusOrMinus(name.baseB, minBaseB, maxBaseB, mat.base.b)
    addPlusOrMinus(name.emissionR, minEmissionR, maxEmissionR, mat.emission.r)
    addPlusOrMinus(name.emissionG, minEmissionG, maxEmissionG, mat.emission.g)
    addPlusOrMinus(name.emissionB, minEmissionB, maxEmissionB, mat.emission.b)

    copy(genes = propMap.toMap)


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

  def percentToValue(prop: Gene, percent: Float)(using DogContext): ValidatedNec[String, Float] =
    if percent < 0 || percent > 1 then
      Validated.Invalid(NonEmptyChain.one(s"Percentage ${percent * 100} outside of range"))
    else
      boundsFor(prop).toRight(s"No bounds for gene property $prop").toValidatedNec.map: (minVal, maxVal) =>
        prop match
          case _: PlusMinusGene =>
            (percent * (minVal + maxVal)) - minVal
          case _: GeneticProperty =>
            (percent * (maxVal - minVal)) + minVal


  def updatedPercent(prop: Gene, percent: Float)(using DogContext): ValidatedNec[String, MasterDogGene] =
    percentToValue(prop, percent).andThen: v =>
      val (minVal, maxVal) = boundsFor(prop).get
      prop match
        case x: PlusMinusGene =>
          updatePlusMinus(x, v, minVal, maxVal)
        case x: GeneticProperty =>
          updateFloatValue(x, v, minVal, maxVal)

  def inferPercent(prop: Gene & HasDefiniteBounds): Float =
    getPercent(prop, prop.minBound, prop.maxBound)

  def inferValue(prop: Gene & HasDefiniteBounds): Float =
    prop match
      case x: (PlusMinusGene & HasDefiniteBounds) => inferPlusMinus(x)
      case x: (GeneticProperty & HasDefiniteBounds) => inferFloatFromGene(x)


  private def getSizeForRepeatingSizeFloat(sizeFloat: Int): Int =
    sizeFloat match
      case 0 => 64
      case 1 => 128
      case _ =>
        println("invalid sizeFloat")
        64

  private def getSplotchWidthFromFloat(f: Float): Int =
    if f <= Dog.splotchChance10 then
      10
    else if f <= Dog.splotchChance64 then
      64
    else if f <= Dog.splotchChance128 then
      128
    else
      256

  private def getSeededRandomInt(startingValue: Int, min: Int, max: Int)(using random: Net5Random): Int =
    var num = random.next(min, max)
    num = (num + startingValue) % (max + 1)
    if num < min || num > max then
      println("invalid random value")
    num

  private def getSeededRandomFloat(startingValue: Float, min: Float, max: Float)(using random: Net5Random): Float =
    var num = GayMath.clampf(random.next(math.round(min), math.round(max) + 1), min, max)
    num = GayMath.clampf((num + startingValue) % (max + 0.1f), min, max)
    if num < min || num > max then
      println("invalid random value")
    num

  private def generateSplotchInfo(using random: Net5Random): SplotchPatternInfo =
    val a = getSeededRandomInt(getIntFromGene(GeneticProperty.PatternFlipX, 0, 1), 0, 1)
    val b = getSeededRandomInt(getIntFromGene(GeneticProperty.PatternFlipY, 0, 1), 0, 1)
    val c = getFloatFromGene(GeneticProperty.PatternInfo, Dog.splotchSizeMin, Dog.splotchSizeMax)
    val c2 = getSplotchWidthFromFloat(getSeededRandomFloat(c, Dog.splotchSizeMin, Dog.splotchSizeMax))
    val d = getSeededRandomFloat(getFloatFromGene(GeneticProperty.PatternInfo, 0f, 100f), 0f, 100f)
    val e = getSeededRandomFloat(getFloatFromGene(GeneticProperty.PatternInfo, 0f, 100f), 0f, 100f) / 100f
    val f = getSeededRandomFloat(getFloatFromGene(GeneticProperty.PatternInfo, 0f, 100f), 0f, 100f) / 100f
    SplotchPatternInfo(a, b, c2, d, e, f)

  private def generateStripeInfo(using random: Net5Random): StripePatternInfo =
    val c = getSeededRandomFloat(getFloatFromGene(GeneticProperty.PatternInfo, 0f, Dog.stripeInfoSize), 0f, Dog.stripeInfoSize)
    val d = getSeededRandomFloat(getFloatFromGene(GeneticProperty.PatternInfo, 0f, Dog.stripeInfoSize), 0f, Dog.stripeInfoSize)
    val e = getSeededRandomFloat(getFloatFromGene(GeneticProperty.PatternInfo, 0f, Dog.stripeInfoSize), 0f, Dog.stripeInfoSize)
    // hard coded count of number of left caps - 1 which is just 1
    val num = 1
    val f = getSeededRandomInt(getIntFromGene(GeneticProperty.PatternInfo, 0f, num), 0, num)
    StripePatternInfo(0, 0, c, d, e, f)

  private def getNumSizesForRepeatingType(kind: Int): Int =
    kind match
      case 0 => 1
      case 1 => 2
      case 2 => 1
      case 3 => 2
      case 4 => 1
      case _ =>
        println("invalid repeating type")
        1

  private def generateRepeatingInfo(loop: Int)(using random: Net5Random): RepeatingPatternInfo =
    val a = getSeededRandomInt(getIntFromGene(GeneticProperty.PatternFlipX, 0f, 1f), 0, 1)
    val b = getSeededRandomInt(getIntFromGene(GeneticProperty.PatternFlipY, 0f, 1f), 0, 1)
    val (c, d) =
      if loop == 0 then
        val intFromGene = getIntFromGene(GeneticProperty.PatternInfo, 0, Dog.numRepeatingTypes - 1)
        val cc = getSeededRandomInt(intFromGene, 0, Dog.numRepeatingTypes - 1)
        val intFromGene2 = getIntFromGene(GeneticProperty.PatternInfo, 0, getNumSizesForRepeatingType(cc) - 1)
        val dd = getSizeForRepeatingSizeFloat(getSeededRandomInt(intFromGene2, 0, getNumSizesForRepeatingType(cc) - 1))
        (cc, dd)
      else
        val cc = getSeededRandomInt(getIntFromGene(GeneticProperty.PatternInfo, 0f, 100f), 0, 100)
        val dd = getSeededRandomInt(getIntFromGene(GeneticProperty.PatternInfo, 0f, 100f), 0, 100)
        (cc, dd)

    val e = getSeededRandomInt(getIntFromGene(GeneticProperty.PatternInfo, 0f, 100f), 0, 100)
    val f = getSeededRandomInt(getIntFromGene(GeneticProperty.PatternInfo, 0f, 100f), 0, 100)
    RepeatingPatternInfo(a, b, c, d, e, f)


  private def fillPatternInfo(using random: Net5Random): List[PatternInfo] =
    (0 until Dog.patternNumMax).map: i =>
      val splotchInfo = generateSplotchInfo
      val stripeInfo = generateStripeInfo
      val repeatingPatternInfo = generateRepeatingInfo(i)
      PatternInfo(splotchInfo, stripeInfo, repeatingPatternInfo)
    .toList

  def patternType: PatternType =
    if this.domRecPropertyStatus(DomRecGeneProperty.NoPattern) then
      PatternType.None
    else if this.domRecPropertyStatus(DomRecGeneProperty.SplotchPattern) then
      PatternType.Splotches
    else if this.domRecPropertyStatus(DomRecGeneProperty.StripePattern) then
      PatternType.Stripes
    else if this.domRecPropertyStatus(DomRecGeneProperty.RepeatingPattern) then
      PatternType.Repeating
    else
      PatternType.None

  def getPartMaterial(part: DogMaterialPart): CalculatedMaterial =
    import part.default.*
    val metallic = minMetallic + inferPlusMinus(part.metallic)
    val glossiness = minGlossiness + inferPlusMinus(part.glossiness)
    val baseR = minBaseR + inferPlusMinus(part.baseR)
    val baseG = minBaseG + inferPlusMinus(part.baseG)
    val baseB = minBaseB + inferPlusMinus(part.baseB)
    val base = ColorF(baseR, baseG, baseB)
    val emiR = minEmissionR + inferPlusMinus(part.emissionR)
    val emiG = minEmissionG + inferPlusMinus(part.emissionG)
    val emiB = minEmissionB + inferPlusMinus(part.emissionB)
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

    val floatMap = mut.LinkedHashMap[Gene, CalculatedValue]()
    val integralMap = mut.LinkedHashMap[Gene, Int]()

    Gene.integralValues.foreach:
      case x: HasDefiniteBounds =>
        integralMap(x) = inferIntFromGene(x)
      case _ => ()

    integralMap(GeneticProperty.LegPairsFront) = frontLegPairs
    integralMap(GeneticProperty.LegPairsBack) = backLegPairs

    Gene.floatValues.foreach: x =>
      this.boundsFor(x) match
        case Some(v) =>
          floatMap(x) = CalculatedValue(getValue(x, v._1, v._2), getPercent(x, v._1, v._2))
        case _ => ()
    

    
    val bodyMat = getPartMaterial(DogMaterialPart.Body)
    val legMat = getPartMaterial(DogMaterialPart.Legs)
    val noseEarMat = getPartMaterial(DogMaterialPart.EarsNose)

    CalculatedGenes(
      bodyMat = bodyMat,
      legColor = legMat,
      noseEarColor = noseEarMat,
      floatItems = floatMap.to(collection.immutable.ListMap),
      integralItems = integralMap.to(collection.immutable.ListMap),
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
    for (gene <- Gene.genes) {
      gene.geneType match
        case SDogGeneType.Super(_) => ()
        case _ =>
          gene match
            case x: PlusMinusGene =>
              stringBuilder.append(genes(x.plus))
              stringBuilder.append(genes(x.minus))
            case x: PlainGeneticProperty =>
              stringBuilder.append(genes(x))
    }
    Gene.genes.foreach { gene =>
      gene.geneType match
        case SDogGeneType.Super(_) =>
          stringBuilder.append(separatorSymbol)
          gene match
            case x: PlusMinusGene =>
              stringBuilder.append(genes(x.plus))
              stringBuilder.append(separatorSymbol)
              stringBuilder.append(genes(x.minus))
            case x: PlainGeneticProperty =>
              stringBuilder.append(genes(x))
        case _ => ()
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

  def fromGenes(inDogGene: String, domRecGene: String): MasterDogGene = {
    var dogGene = inDogGene
    val randomSeed = dogGene.substring(0, randomSeedSize)
    var currentSuperIndex = 0
    var currentStandardIndex = randomSeedSize
    // val geneValues = mut.Map[GeneticProperty, DogGene]()
    val geneticHolders = mut.Map[GeneticProperty, String]()
    val domRecPropertyStatus = mut.Map[DomRecGeneProperty, Boolean]()
    val domRecGenes = domRecGene.grouped(2).zip(DomRecGeneStatic.values).map { (it, domRecGene) =>
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

    def mapGene(gene: GeneticProperty): Unit =
      gene.geneType match
        case SDogGeneType.Super(_) =>
          var num = dogGene.indexOf(separatorSymbol, currentSuperIndex) + 1
          if (num == 0) {
            // WHAT THE FREAK
            println("Missing super type, fixie wixieing")
            dogGene += generateBaseGeneOfSize(gene.defaultLen)
            num = dogGene.indexOf(separatorSymbol, currentSuperIndex) + 1
          }
          var num2 = dogGene.indexOf(separatorSymbol, num)
          if (num2 == -1) {
            num2 = dogGene.length
          }

          geneticHolders(gene) = dogGene.substring(num, num2)
          currentSuperIndex = num
        case _ =>
          val value = dogGene.substring(currentStandardIndex, currentStandardIndex + gene.defaultLen)
          geneticHolders(gene) = value
          currentStandardIndex += gene.defaultLen
    Gene.genes.foreach:
      case x: PlusMinusGene =>
        mapGene(x.plus)
        mapGene(x.minus)
      case x: PlainGeneticProperty =>
        mapGene(x)
    MasterDogGene(dogGene.substring(0, randomSeedSize), geneticHolders.toMap, domRecGenes, domRecPropertyStatus.toMap)
  }

  private def generateBaseGeneOfSize(size: Int, addSeperator: Boolean = true): String = {
    (if (addSeperator) separatorSymbol else "") + "0".repeat(size)
  }

}
