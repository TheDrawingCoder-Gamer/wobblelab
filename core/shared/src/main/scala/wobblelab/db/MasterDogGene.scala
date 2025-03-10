package net.bulbyvr
package wobblelab
package db



import net.bulbyvr.wobblelab.db.MasterDogGene.{minusString, randomSeedSize, separatorSymbol}

import scala.collection.mutable as mut

case class RawGene(
                  dogGene: String,
                  domRecGene: String
                  )

case class MasterDogGene
  ( randomSeed: String,
    geneValues: Map[GeneticProperty, DogGene],
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

  def updatedGeneString(key: GeneticProperty, value: String): MasterDogGene = {
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
  def getExpectedGeneSize(key: GeneticProperty): Int = {
    geneticHolders.get(key).flatMap {
      case s: DogGeneHolder.Super => Some(s.originalLength)
      case _ => None
    }.getOrElse(0)
  }
  def getDynamicSeparatedFloatFromGene(key: GeneticProperty, minVal: Float, maxVal: Float, negativeValue: Boolean = false): Option[Float] = {
    getGeneString(key).map { geneString =>
      val expectedGeneSize = getExpectedGeneSize(key)
      val length = geneString.length
      var num = maxVal + getMaxValIncrease(key) * (length - expectedGeneSize).toFloat
      if (num < maxVal) {
        // ???
        num = maxVal
      }
      if (negativeValue && length != expectedGeneSize) {
        num = maxVal
      }
      val floatFromGeneSequence = DogMath.getFloatFromGeneSequence(geneString, minVal, num)
      floatFromGeneSequence
    }
  }

  def getDynamicSeparatedIntFromGene(key: GeneticProperty, minVal: Float, maxVal: Float): Option[Int] = {
    getGeneString(key).map { geneString =>
      val expectedGeneSize = getExpectedGeneSize(key)
      val length = geneString.length
      val flag = length >= expectedGeneSize
      val maxVal2 = maxVal + getMaxValIncrease(key) * (length - expectedGeneSize).toFloat
      val num = if (!flag) Math.round(minVal).toInt else Math.round(DogMath.getFloatFromGeneSequence(geneString, minVal, maxVal2))

      num
    }
  }

  def getFloatFromGene(key: GeneticProperty, minVal: Float, maxVal: Float): Option[Float] = {
    getGeneString(key).map { geneString =>
      val floatFromGeneSequence = DogMath.getFloatFromGeneSequence(geneString, minVal, maxVal)
      floatFromGeneSequence
    }
  }

  def getIntFromGene(key: GeneticProperty, minVal: Float, maxVal: Float): Option[Int] = {
    getFloatFromGene(key, minVal, maxVal).map(Math.round)
  }

  def getMaxValIncrease(key: GeneticProperty): Float = {
    geneticHolders.get(key).flatMap {
      case s: DogGeneHolder.Super => Some(s.maxValIncrease)
      case _ => None
    }.getOrElse(0f)
  }

  def calculateGenes(): CalculatedGenes = {
    def calculatePlusMinus(key: String, minVal: Float, maxVal: Float, isSuper: Boolean = true): Float = {
      val plusProp = GeneticProperty.valueOf(key + MasterDogGene.plusString)
      val minusProp = GeneticProperty.valueOf(key + MasterDogGene.minusString)
      val plusValue = if (isSuper) getDynamicSeparatedFloatFromGene(plusProp, 0, maxVal) else getFloatFromGene(plusProp, 0, maxVal)
      val minusValue = if (isSuper) getDynamicSeparatedFloatFromGene(minusProp, 0, minVal, true) else getFloatFromGene(minusProp, 0, minVal)
      plusValue.get - minusValue.get
    }
    def calculateStandard(key: GeneticProperty, minVal: Float, maxVal: Float, isSuper: Boolean = true): Float = {
      val value = if (isSuper) getDynamicSeparatedFloatFromGene(key, minVal, maxVal) else getFloatFromGene(key, minVal, maxVal)
      value.get
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

    val earType = {
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
    }
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
      if (centerHorn && traditionalHorns)
        HornPlacement.HornPlacementAll
      else if (centerHorn)
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
    val headNumber =
      math.min(this.getDynamicSeparatedIntFromGene(GeneticProperty.HeadNumber, Dog.headNumMin, Dog.headNumMax).get, Dog.headCap)
    val tailNumber =
      this.getDynamicSeparatedIntFromGene(GeneticProperty.TailNum, Dog.tailNumMin, Dog.tailNumMax).get
    var frontLegPairs =
      math.max(1,
        math.floor(this.getDynamicSeparatedFloatFromGene(GeneticProperty.LegPairsFront, Dog.legNumberMin, Dog.legNumberMax).get / Dog.legNumberIncreaseRate).toInt
      )
    var backLegPairs =
      math.max(1,
        math.floor(this
          .getDynamicSeparatedFloatFromGene(GeneticProperty.LegPairsBack, Dog.legNumberMin, Dog.legNumberMax)
          .get / Dog.legNumberIncreaseRate).toInt
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
    
    CalculatedGenes(
      bodyMat = CalculatedMaterial.DEFAULT,
      legColor = CalculatedMaterial.DEFAULT,
      noseEarColor = CalculatedMaterial.DEFAULT,
      floatItems = Map(),
      headNumber = headNumber,
      tailNumber = tailNumber,
      wingNumber = wingNumber,
      frontLegPairs = frontLegPairs,
      backLegPairs = backLegPairs,
      earType, hornType, hornPlacement, noseType, tailType, wingType
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
    val geneValues = mut.Map[GeneticProperty, DogGene]()
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
    MasterDogGene(dogGene.substring(0, randomSeedSize), geneValues.toMap, geneticHolders.toMap, domRecGenes, domRecPropertyStatus.toMap)
  }

  private def generateBaseGeneOfSize(size: Int, addSeperator: Boolean = true): String = {
    (if (addSeperator) separatorSymbol else "") + "0".repeat(size)
  }

}
