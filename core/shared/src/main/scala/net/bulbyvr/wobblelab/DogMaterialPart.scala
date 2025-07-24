package net.bulbyvr.wobblelab

import net.bulbyvr.wobblelab.db.{GeneticProperty, HasDefiniteBounds, PlusMinusGene}

sealed trait DogMaterialPart:
  val metallic: PlusMinusGene & HasDefiniteBounds
  val glossiness: PlusMinusGene & HasDefiniteBounds
  val baseR: PlusMinusGene & HasDefiniteBounds
  val baseG: PlusMinusGene & HasDefiniteBounds
  val baseB: PlusMinusGene & HasDefiniteBounds
  val emissionR: PlusMinusGene & HasDefiniteBounds
  val emissionG: PlusMinusGene & HasDefiniteBounds
  val emissionB: PlusMinusGene & HasDefiniteBounds
  
  val default: Dog.Material

  val display: String

object DogMaterialPart:
  case object Body extends DogMaterialPart:
    override val display: String = "Body"

    override val metallic = GeneticProperty.BodyMetallic
    override val glossiness = GeneticProperty.BodyGloss
    override val baseR = GeneticProperty.BodyColorR
    override val baseG = GeneticProperty.BodyColorG
    override val baseB = GeneticProperty.BodyColorB
    override val emissionR = GeneticProperty.BodyEmissionColorR
    override val emissionG = GeneticProperty.BodyEmissionColorG
    override val emissionB = GeneticProperty.BodyEmissionColorB

    override val default: Dog.Material = Dog.defaultMaterials.body

  case object Legs extends DogMaterialPart:
    override val display: String = "Legs/Head"

    override val metallic = GeneticProperty.LegMetallic
    override val glossiness = GeneticProperty.LegGloss
    override val baseR = GeneticProperty.LegColorR
    override val baseG = GeneticProperty.LegColorG
    override val baseB = GeneticProperty.LegColorB
    override val emissionR = GeneticProperty.LegEmissionColorR
    override val emissionG = GeneticProperty.LegEmissionColorG
    override val emissionB = GeneticProperty.LegEmissionColorB

    override val default: Dog.Material = Dog.defaultMaterials.legs

  case object EarsNose extends DogMaterialPart:
    override val display: String = "Nose/Ears"

    override val metallic = GeneticProperty.NoseEarMetallic
    override val glossiness = GeneticProperty.NoseEarGloss
    override val baseR = GeneticProperty.NoseEarColorR
    override val baseG = GeneticProperty.NoseEarColorG
    override val baseB = GeneticProperty.NoseEarColorB
    override val emissionR = GeneticProperty.NoseEarEmissionColorR
    override val emissionG = GeneticProperty.NoseEarEmissionColorG
    override val emissionB = GeneticProperty.NoseEarEmissionColorB

    override val default: Dog.Material = Dog.defaultMaterials.earsNose

  case object Pattern extends DogMaterialPart:
    override val display: String = "Pattern"

    override val metallic = GeneticProperty.PatternMetallic
    override val glossiness = GeneticProperty.PatternSmoothness
    override val baseR = GeneticProperty.PatternColorR
    override val baseG = GeneticProperty.PatternColorG
    override val baseB = GeneticProperty.PatternColorB
    override val emissionR = GeneticProperty.PatternEmissionColorR
    override val emissionG = GeneticProperty.PatternEmissionColorG
    override val emissionB = GeneticProperty.PatternEmissionColorB

    override val default: Dog.Material = Dog.defaultMaterials.bodyPattern

given util.PrettyPrint[DogMaterialPart] = _.display