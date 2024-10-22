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
