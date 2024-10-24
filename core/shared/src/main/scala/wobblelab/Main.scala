package net.bulbyvr
package wobblelab

import cats.parse.{Parser, Parser0}
import cats.implicits.*
import magic.{*, given}

import java.text.DecimalFormat
import scala.annotation.experimental
import io.circe.syntax.*
import io.circe.*
import io.circe.Decoder.Result
import wobblelab.util.*


object DogRegistry {
  val currentGeneVersion: Int = 3
  val dogExportSeperator: String = "^"
  def exportDog(rawDog : RawDog) : String = {
    val dogGene = DogMath.geneticEncode(rawDog.dogGene)
    val domRecGene = DogMath.geneticEncode(rawDog.domRecGene.replace('A', '1').replace('a', '0'))
    val floatFormatter = new DecimalFormat("0.#####")
    DogMath.scramble(dogExportSeperator.repeat(3) + rawDog.geneVersion.versionId.toString + dogExportSeperator +
      dogGene + dogExportSeperator + domRecGene + dogExportSeperator + rawDog.dogAge.toString.toUpperCase +
      dogExportSeperator
      + floatFormatter.format(rawDog.dogAgeProgress)
      + dogExportSeperator
      + floatFormatter.format(rawDog.eolModifier)
      + dogExportSeperator
      + floatFormatter.format(rawDog.lifeExtension)
      + dogExportSeperator
      + rawDog.personality.serialized
      + dogExportSeperator
      + rawDog.dogName)
  }
  def importDog(dogCode : String): Option[RawDog] = {
    var text = DogMath.unscramble(dogCode)
    val sectionParser = Parser.until(Parser.string(dogExportSeperator)) <* Parser.char('^').backtrack.orElse(Parser.end)
    val flagsParser = Parser.string(dogExportSeperator.repeat(3)).as((true, true, true)).backtrack
                      | Parser.string(dogExportSeperator.repeat(2)).as((true, true, false)).backtrack
                      | Parser.string(dogExportSeperator.repeat(1)).as((true, false, false)).backtrack
                      | Parser.pure((false, false, false))
    val parser = flagsParser.flatMap { (flag, flag2, flag3) =>

        val geneParser =
          if flag3 then
            GeneVersion.parser
          else
            Parser.pure(GeneVersion.Zero)
        val flag2thingies =
          if flag2 then
            sectionParser.mapFilter(_.toFloatOption)
          else
            Parser.pure(0.0f)
        (geneParser <* Parser.char('^')
        ,sectionParser.map(DogMath.geneticDecode)
        ,sectionParser.map(DogMath.geneticDecode).map(_.replace('0', 'a').replace('1', 'A'))
        ,sectionParser.map(DogAge.parseString)
        ,sectionParser.mapFilter(_.toFloatOption)
        ,flag2thingies
        ,flag2thingies
        ,if flag then DogPersonality.parser <* Parser.char('^') else Parser.pure(DogPersonality.empty)
        , Parser.until(Parser.end)
        ).mapN(RawDog.apply)
    }
    parser.parseAll(text).toOption

  }
}

enum GeneVersion(val versionId: Int, val name: String) {
  case All extends GeneVersion(-1, "ALL")
  case Zero extends GeneVersion(0, "ZERO")
  case One extends GeneVersion(1, "ONE")
  case Two extends GeneVersion(2, "TWO")
  case Three extends GeneVersion(3, "THREE")
}

given codecGeneVersion: Codec[GeneVersion] with {
  override def apply(a: GeneVersion): Json = a.versionId.asJson

  override def apply(c: HCursor): Result[GeneVersion] = c.as[Int].flatMap { idx =>
    GeneVersion.values.find(_.versionId == idx) match {
      case Some(value) => Right(value)
      case None => Left(DecodingFailure(s"No GeneVersion for version $idx", c.history))
    }
  }
}


object GeneVersion {
  val parser =
    Parser.until(Parser.char('^')).mapFilter(_.toIntOption).mapFilter(ofInt)

  def ofInt(i: Int): Option[GeneVersion] = {
    i match {
      case -1 => Some(All)
      case 0 => Some(Zero)
      case 1 => Some(One)
      case 2 => Some(Two)
      case 3 => Some(Three)
      case _ => None
    }
  }

  def parseString(str: String): Option[GeneVersion] =
    str.toIntOption match {
      case Some(i) => ofInt(i)
      case None =>
        str match {
          case "ALL" => Some(All)
          case "ZERO" => Some(Zero)
          case "ONE" => Some(One)
          case "TWO" => Some(Two)
          case "THREE" => Some(Three)
          case _ => None
        }
    }
}

enum EnergyPersonality(id: Int) {
  case Goof extends EnergyPersonality(0)
  case StandardEnergy extends EnergyPersonality(1)
  case Layabout extends EnergyPersonality(2)
}
object EnergyPersonality {
  def parseString(str: String): EnergyPersonality = {
    EnergyPersonality.fromOrdinal(str.toInt)
  }
}

enum LoudnessPersonality(id: Int) {
  case StandardLoud extends LoudnessPersonality(0)
  case Loud extends LoudnessPersonality(1)
  case Quiet extends LoudnessPersonality(2)
}
object LoudnessPersonality {
  def parseString(str: String): LoudnessPersonality = {
    str.toIntOption.map { it => LoudnessPersonality.fromOrdinal(it) }.getOrElse(LoudnessPersonality.StandardLoud)
  }
}

enum MischiefPersonality(id: Int) {
  case Polite extends MischiefPersonality(0)
  case StandardMischief extends MischiefPersonality(1)
  case Mischevious extends MischiefPersonality(2)
}
object MischiefPersonality {
  def parseString(str: String) =
    MischiefPersonality.fromOrdinal(str.toInt)
}
enum PettablePersonality(id: Int) {
  case LikesPetting extends PettablePersonality(0)
  case DislikesPetting extends PettablePersonality(1)
}
object PettablePersonality {
  def parseString(str: String) =
    PettablePersonality.fromOrdinal(str.toInt)
}
enum FoodPersonality(id: Int) {
  case Glutton extends FoodPersonality(0)
  case Standard extends FoodPersonality(1)
  case FoodAverse extends FoodPersonality(2)
}
object FoodPersonality {
  def parseString(str: String) =
    FoodPersonality.fromOrdinal(str.toInt)
}
enum NicenessPersonality {
  case Nice
  case NiceStandard
  case Mean
}
object NicenessPersonality {
  def parseString(str: String) =
    NicenessPersonality.fromOrdinal(str.toInt)
}
enum SocialPersonality {
  case Social
  case SocialStandard
  case Aloof
}
object SocialPersonality {
  def parseString(str: String) =
    SocialPersonality.fromOrdinal(str.toInt)
}
enum DogAge {
  case Empty
  case Puppy
  case Child
  case Teen
  case YoungAdult
  case Adult
  case Ancient
}
object DogAge {
  def parseString(str: String) =
    str.toIntOption match {
      case Some(i) => DogAge.fromOrdinal(i)
      case None =>
        str match {
          case "NONE" => Empty
          case "PUPPY" => Puppy
          case "CHILD" => Child
          case "TEEN" => Teen
          case "YOUNG_ADULT" => YoungAdult
          case "ADULT" => Adult
          case "ANCIENT" => Ancient
          case _ => throw IllegalArgumentException("invalid life stage")
        }
    }
}
case class DogPersonality
  (social: SocialPersonality = SocialPersonality.SocialStandard,
   energy: EnergyPersonality = EnergyPersonality.StandardEnergy,
   food: FoodPersonality = FoodPersonality.Standard,
   mischief: MischiefPersonality = MischiefPersonality.StandardMischief,
   niceness: NicenessPersonality = NicenessPersonality.NiceStandard,
   pettable: PettablePersonality = PettablePersonality.LikesPetting,
   loudness: LoudnessPersonality = LoudnessPersonality.StandardLoud) {
  lazy val serialized =
    social.ordinal.toString + energy.ordinal.toString + food.ordinal.toString + mischief.ordinal.toString + niceness.ordinal.toString + pettable.ordinal.toString +
    loudness.ordinal.toString
}
object DogPersonality {
  val parser =
    (Parser.length(1).map(SocialPersonality.parseString)
    ,Parser.length(1).map(EnergyPersonality.parseString)
    ,Parser.length(1).map(FoodPersonality.parseString)
    ,Parser.length(1).map(MischiefPersonality.parseString)
    ,Parser.length(1).map(NicenessPersonality.parseString)
    ,Parser.length(1).map(PettablePersonality.parseString)
    ,Parser.char('^').as(LoudnessPersonality.StandardLoud).backtrack.orElse(Parser.length(1).map(LoudnessPersonality.parseString))
    ).mapN(DogPersonality.apply)
  val empty =
    DogPersonality(SocialPersonality.SocialStandard, EnergyPersonality.StandardEnergy, FoodPersonality.Standard, MischiefPersonality.StandardMischief, NicenessPersonality.NiceStandard,
    PettablePersonality.LikesPetting, LoudnessPersonality.StandardLoud)
}
// Not what you think you sicko!!!
case class RawDog(geneVersion: GeneVersion, dogGene: String, domRecGene: String, dogAge: DogAge, dogAgeProgress: Float, eolModifier: Float, lifeExtension: Float, personality: DogPersonality, dogName : String) {
  def toGameDog: GameDog = {
    val genes = db.MasterDogGene.fromGenes(dogGene, domRecGene)
    GameDog(dogName, genes, personality, dogAge, dogAgeProgress, eolModifier, lifeExtension)
  }
}
object RawDog {
  @experimental
  object experimental {
    extension (self: RawDog) {
      def toDog: Option[Dog] = {
        val gene0res = DogGene0.parser.parse(self.dogGene)
        gene0res match {
          case Left(e) =>
            None
          case Right((rest, prop0genes)) =>
            DogGene1.parser.parseAll(rest) match {
              case Left(e) =>
                None
              case Right(prop1genes) =>
                val domRecGenes = DomRecGene.parser.parseAll(self.domRecGene).toOption.get
                Some(Dog(self.personality, prop0genes, prop1genes, domRecGenes, self.dogAge, self.dogAgeProgress, self.eolModifier, self.lifeExtension, self.dogName))
            }
        }
      }
    }
  }
}
case class Dog(personality: DogPersonality, geneProp0 : DogGene0, geneProp1: DogGene1, domRecGene : DomRecGene, age: DogAge, ageProgress: Float,
  eolModifier: Float, lifeExtension: Float, dogName : String) {
  def asRawDog: RawDog = {
    val dogGene = geneProp0.serialized + "|" + geneProp1.serialized
    RawDog(GeneVersion.Three, dogGene, domRecGene.serialized, age, ageProgress, eolModifier, lifeExtension, personality, dogName)
  }
}
object Dog {
  val colorMax: Float = 1
  val emissionMax: Float = 0.95
  val glossMax: Float = 0.95
  val metallicMax: Float = 0.95
  // I think this means it is uncapped?
  val hornSizeMin: Float = 0.5
  val hornSizeMax: Float = 0.5

  val headSizeMin: Float = 0.5
  val headSizeMax: Float = 0.2

  val headSizeCap: Float = 0.5

  val headNumMin: Int = 1
  val headNumMax: Int = 1

  val headCap: Int = 10

  val bodyScaleXMin: Float = 0.75
  val bodyScaleXMax: Float = 0.75
  val bodyScaleXCap: Float = 2

  val bodyScaleYZMin: Float = 0.3
  val bodyScaleYZMax: Float = 0.3
  val bodyScaleYZCap: Float = 0.6

  val bodyScaleZMin: Float = 0.5
  val bodyScaleZMax: Float = 1
  val bodyScaleZCap: Float = 2

  val tailScaleMin: Float = 0.5
  val tailScaleMax: Float = 1.75
  val tailScaleCap: Float = 2.25

  val tailNumMin: Float = 1
  val tailNumMax: Float = 1
  val tailNumCap: Int = 4

  val wingScaleMin: Float = 0.75
  val wingScaleMax: Float = 0.5

  val wingNumberMin: Float = 1
  val wingNumberMax: Float = 1
  val wingNumberCap: Int = 4

  // wing z??

  val legGirthMin: Float = 0.55
  val legGirthMax: Float = 1
  val legGirthMaxPuppy: Float = 0.6

  val legLengthTopMin: Float = 0.15
  val legLengthTopMax: Float = 0.75
  val legLengthBotMin: Float = 0.25
  val legLengthBotMax: Float = 0.75

  val stanceWidthMin: Float = 1
  val stanceWidthMax: Float = 1
  val minLegSeparation: Float = 0.1

  val legNumberMin: Float = 1
  val legNumberMax: Float = 2

  // ???
  val legNumberCap: Int = 6
  val legNumberHardCap: Int = 30

  val textureAlphaMin: Float = 0
  val textureAlphaMax: Float = 1

  val textureMetallicMin: Float = 0
  val textureMetallicMax: Float = 0.75

  val textureSmoothnessMin: Float = 0
  val textureSmoothnessMax: Float = 0.75

  val dogScaleGlobalMin: Float = 0.5
  val dogScaleGlobalMax: Float = 0.5
  val dogScaleGlobalCap: Float = 0.5

  val snoutModRotYMin: Float = 95
  val snoutModRotYMax: Float = 65

  val snoutModLenMin: Float = 0.25
  val snoutModLenMax: Float = 0.5

  val snoutModScaleMin: Float = 0.5
  val snoutModScaleMax: Float = 1
  
  trait Material {
    val base: util.ColorF
    val emission: util.ColorF
    val glossiness: Float
    val metallic: Float

    def minBaseR: Float = base.r
    def maxBaseR: Float = Math.max(colorMax - base.r, 0f)
    def minBaseG: Float = base.g
    def maxBaseG: Float = Math.max(colorMax - base.g, 0f)
    def minBaseB: Float = base.b
    def maxBaseB: Float = Math.max(colorMax - base.b, 0f)

    def minEmissionR: Float = emission.r
    def maxEmissionR: Float = Math.max(emissionMax - emission.r, 0f)
    def minEmissionG: Float = emission.g
    def maxEmissionG: Float = Math.max(emissionMax - emission.g, 0f)
    def minEmissionB: Float = emission.b
    def maxEmissionB: Float = Math.max(emissionMax - emission.b, 0f)

    def minGlossiness: Float = glossiness
    def maxGlossiness: Float = Math.max(glossMax - glossiness, 0f)

    def minMetallic: Float = metallic
    def maxMetallic: Float = Math.max(metallicMax - metallic, 0f)
  }
  object defaultMaterials {
    object body extends Material {
      override val base: util.ColorF = util.ColorF(0.9934078, 0.52205884, 1)
      override val emission: util.ColorF = util.ColorF(0.4967039, 0.26102942, 0.5)
      override val glossiness: Float = 0.675
      override val metallic: Float = 0.083
    }
    object bodyPattern extends Material {
      override val base: util.ColorF = util.ColorF(0.9934078, 0.52205884, 1)
      override val emission: util.ColorF = util.ColorF(0.4967039, 0.26102942, 0.5)
      override val glossiness: Float = 0.675
      override val metallic: Float = 0.083

      override def minMetallic: Float = textureMetallicMin
      override def maxMetallic: Float = Math.max(textureMetallicMax - metallic, 0f)

      override def minGlossiness: Float = textureSmoothnessMin
      override def maxGlossiness: Float = Math.max(textureSmoothnessMax - glossiness, 0f)

    }
    object earsNose extends Material {
      override val base: ColorF = ColorF(0.12941177, 0.12941177, 0.12941177)
      override val emission: ColorF = ColorF(0, 0, 0)
      override val glossiness: Float = 0
      override val metallic: Float = 0
    }
    object legs extends Material {
      override val base: ColorF = ColorF(0.6691177, 1, 0.98748773)
      override val emission: ColorF = ColorF(0, 0.3965516, 0.5)
      override val glossiness: Float = 0.675
      override val metallic: Float = 0.083
    }

  }
  lazy val randy: Dog =
    Dog(
      DogPersonality(),
      DogGene0(
        "0100100010",
        ShinyGene(),
        ColorGene(),
        ShinyGene(),
        ColorGene(),
        ShinyGene(),
        ColorGene(),
        DualGene(),
        DualGene(),
        DualGene(),
        DualGene(),
        DualGene(),
        DualGene(),
        DualGene(),
        DualGene(),
        DualGene(),
        DualGene(),
        DualGene(),
        ColorGene(),
        "00000",
        ShinyGene(),
        "00000",
        "0000000000000000000000000",
        "0000000000000000000000000",
        "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
        ""
      ),
      DogGene1(
        "00000",
        DualGene(),
        DualGene(),
        DualGene(),
        DualGene(),
        DualGene(),
        DualGene(),
        "00000",
        "00000",
        DualGene(),
        DualGene(),
        DualGene(),
        DualGene(),
        DualGene(),
        DualGene(),
        "000000000000000",
        "000000000000000"
      ),
      DomRecGene(
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.het,
        DomRecAllele.het,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.het,
        DomRecAllele.het,
        DomRecAllele.dom,
        DomRecAllele.sub,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        // todo: get real values for this
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom,
        DomRecAllele.dom
      ),
      DogAge.Adult,
      0.0166659,
      0,
      0,
      "Randy"
    )
}
case class DogGene0(
                     randomSeed : String,
                     bodyShiny : ShinyGene,
                     bodyColor : ColorGene,
                     legShiny : ShinyGene,
                     legColor : ColorGene,
                     noseShiny : ShinyGene,
                     noseColor : ColorGene,
                     noseModA : DualGene,
                     hornSize : DualGene,
                     earModA : DualGene,
                     // left positive
                     earCurl : DualGene,
                     snoutModA : DualGene,
                     snoutModB : DualGene,
                     snoutModC : DualGene,
                     headSize : DualGene,
                     wingSize : DualGene,
                     stanceWidthFront : DualGene,
                     stanceWidthBack : DualGene,
                     patternColor : ColorGene,
                     patternAlpha : String,
                     patternShiny : ShinyGene,
                     patternNum : String,
                     patternFlipX : String,
                     patternFlipY : String,
                     patternInfo : String,
                     extraBits : String
      ) {
    lazy val serialized: String =
      randomSeed + bodyShiny.serialized + bodyColor.serialized +
      legShiny.serialized + legColor.serialized + noseShiny.serialized +
      noseColor.serialized + noseModA.gene0Serialized + hornSize.gene0Serialized +
      earModA.gene0Serialized + earCurl.gene0Serialized + snoutModA.gene0Serialized +
      snoutModB.gene0Serialized + snoutModC.gene0Serialized + headSize.gene0Serialized +
      wingSize.gene0Serialized + stanceWidthFront.gene0Serialized + stanceWidthBack.gene0Serialized +
      patternColor.patternSerialized + patternAlpha + patternShiny.serialized + patternNum +
      patternFlipX + patternFlipY + patternInfo + extraBits
}

object DogGene0 {
  @experimental
  val parser : Parser0[DogGene0] = {
    val sLen = 5
    FunctionHelper.curried(DogGene0.apply).pure[Parser0]
                  .ap(Parser.length(sLen * 2))
                  .ap(ShinyGene.parser)
                  .ap(ColorGene.parser)
                  .ap(ShinyGene.parser)
                  .ap(ColorGene.parser)
                  .ap(ShinyGene.parser)
                  .ap(ColorGene.parser)
                  .ap(DualGene.parserGene0)
                  .ap(DualGene.parserGene0)
                  .ap(DualGene.parserGene0)
                  .ap(DualGene.parserGene0)
                  .ap(DualGene.parserGene0)
                  .ap(DualGene.parserGene0)
                  .ap(DualGene.parserGene0)
                  .ap(DualGene.parserGene0)
                  .ap(DualGene.parserGene0)
                  .ap(DualGene.parserGene0)
                  .ap(DualGene.parserGene0)
                  .ap(ColorGene.parserPattern)
                  .ap(Parser.length(sLen))
                  .ap(ShinyGene.parser)
                  .ap(Parser.length(sLen))
                  .ap(Parser.length(sLen * 5))
                  .ap(Parser.length(sLen * 5))
                  .ap(Parser.length(sLen * 25))
                  .ap(Parser.until0(Parser.char('|')) <* Parser.char('|'))
  }
}


case class DogGene1(
  headNumber : String,
  bodyScaleX : DualGene,
  bodyScaleZ : DualGene,
  bodyScaleY : DualGene,
  bodyScaleYZ : DualGene,
  bodyScaleGlobal : DualGene,
  tailScale : DualGene,
  tailNum : String,
  wingNum : String,
  legScaleXZFront : DualGene,
  legScaleXZBack : DualGene,
  legScaleYFrontTop : DualGene,
  legScaleYFrontBot : DualGene,
  legScaleYBackTop : DualGene,
  legScaleYBackBot : DualGene,
  legPairsFront : String,
  legPairsBack : String
  ) {
  lazy val serialized =
    headNumber +
    "|" + bodyScaleX.gene1Serialized +
    "|" + bodyScaleZ.gene1Serialized +
    "|" + bodyScaleY.gene1Serialized +
    "|" + bodyScaleYZ.gene1Serialized +
    "|" + bodyScaleGlobal.gene1Serialized +
    "|" + tailScale.gene1Serialized +
    "|" + tailNum +
    "|" + wingNum +
    "|" + legScaleXZFront.gene1Serialized +
    "|" + legScaleXZBack.gene1Serialized +
    "|" + legScaleYFrontTop.gene1Serialized +
    "|" + legScaleYFrontBot.gene1Serialized +
    "|" + legScaleYBackTop.gene1Serialized +
    "|" + legScaleYBackBot.gene1Serialized +
    "|" + legPairsFront +
    "|" + legPairsBack
}
object DogGene1 {
  def parser = {
    val sectionParser = Parser.until(Parser.char('|')) <* Parser.char('|')
    ( sectionParser
    , DualGene.parserGene1
    , DualGene.parserGene1
    , DualGene.parserGene1
    , DualGene.parserGene1
    , DualGene.parserGene1
    , DualGene.parserGene1
    , sectionParser
    , sectionParser
    , DualGene.parserGene1
    , DualGene.parserGene1
    , DualGene.parserGene1
    , DualGene.parserGene1
    , DualGene.parserGene1
    , DualGene.parserGene1
    , sectionParser
    , Parser.until(Parser.end) ).mapN(DogGene1.apply)
  }
  def parse(groups : Iterator[String]) = {
    val headNumber = groups.next()
    val bodyScaleX = DualGene.parse(groups)
    val bodyScaleZ = DualGene.parse(groups)
    val bodyScaleY = DualGene.parse(groups)
    val bodyScaleYZ = DualGene.parse(groups)
    val bodyScaleGlobal = DualGene.parse(groups)
    val tailScale = DualGene.parse(groups)
    val tailNum = groups.next()
    val wingNum = groups.next()
    val legScaleXZFront = DualGene.parse(groups)
    val legScaleXZBack = DualGene.parse(groups)
    val legScaleYFrontTop = DualGene.parse(groups)
    val legScaleYFrontBot = DualGene.parse(groups)
    val legScaleYBackTop = DualGene.parse(groups)
    val legScaleYBackBot = DualGene.parse(groups)
    val legPairsFront = groups.next()
    val legPairsBack = groups.next()
    DogGene1(headNumber, bodyScaleX, bodyScaleZ, bodyScaleY, bodyScaleYZ, bodyScaleGlobal, tailScale,
      tailNum, wingNum, legScaleXZFront, legScaleXZBack, legScaleYFrontTop, legScaleYFrontBot, legScaleYBackTop,
      legScaleYBackBot, legPairsFront, legPairsBack)
  }
}
case class ShinyGene(
    metallicPlus : String = "00000",
    metallicMinus : String = "00000",
    glossPlus : String = "00000",
    glossMinus : String = "00000") {
  lazy val serialized =
    metallicPlus + metallicMinus +
    glossPlus + glossMinus
}
object ShinyGene {
  val parser = {
    val sectionParser = Parser.length(5)
    (sectionParser, sectionParser, sectionParser, sectionParser).mapN[Parser, ShinyGene](ShinyGene.apply)
  }
}
case class ColorGene(
    emissionRedPlus : String = "00000",
    emissionRedMinus : String = "00000",
    emissionGreenPlus : String = "00000",
    emissionGreenMinus : String = "00000",
    emissionBluePlus : String = "00000",
    emissionBlueMinus : String = "00000",
    baseRedPlus : String = "00000",
    baseRedMinus : String = "00000",
    baseGreenPlus : String = "00000",
    baseGreenMinus : String = "00000",
    baseBluePlus : String = "00000",
    baseBlueMinus : String = "00000"
  ) {
  lazy val serialized = {
    emissionRedPlus + emissionRedMinus + emissionGreenPlus +
    emissionGreenMinus + emissionBluePlus + emissionBlueMinus +
    baseRedPlus + baseRedMinus + baseGreenPlus +
    baseGreenMinus + baseBluePlus + baseBlueMinus
  }
  lazy val patternSerialized = {
    baseRedPlus + baseRedMinus + baseGreenPlus +
    baseGreenMinus + baseBluePlus + baseBlueMinus +
    emissionRedPlus + emissionRedMinus + emissionGreenPlus +
    emissionGreenMinus + emissionBluePlus + emissionBlueMinus
  }
}

object ColorGene {
  private val parserBase = {
    val sectionParser = Parser.length(5)
    ( sectionParser
    , sectionParser
    , sectionParser
    , sectionParser
    , sectionParser
    , sectionParser
    , sectionParser
    , sectionParser
    , sectionParser
    , sectionParser
    , sectionParser
    , sectionParser )
  }
  val parser : Parser[ColorGene] =
    parserBase.mapN[Parser, ColorGene](ColorGene.apply)
  val parserPattern =
    parserBase.mapN[Parser, ColorGene]( ( g, h, i, j, k, l, a, b, c, d, e, f) =>
        ColorGene(a, b, c, d, e, f, g, h, i, j, k, l) )
  def parsePattern(groups : Iterator[String]) : ColorGene = {
      val bRP = groups.next()
      val bRM = groups.next()
      val bGP = groups.next()
      val bGM = groups.next()
      val bBP = groups.next()
      val bBM = groups.next()
      val emRP = groups.next()
      val emRM = groups.next()
      val emGP = groups.next()
      val emGM = groups.next()
      val emBP = groups.next()
      val emBM = groups.next()
      ColorGene(emRP, emRM, emGP, emGM, emBP, emBM, bRP, bRM, bGP, bGM, bBP, bBM)
  }
  def parse(groups : Iterator[String]) : ColorGene = {
      val emRP = groups.next()
      val emRM = groups.next()
      val emGP = groups.next()
      val emGM = groups.next()
      val emBP = groups.next()
      val emBM = groups.next()
      val bRP = groups.next()
      val bRM = groups.next()
      val bGP = groups.next()
      val bGM = groups.next()
      val bBP = groups.next()
      val bBM = groups.next()
      ColorGene(emRP, emRM, emGP, emGM, emBP, emBM, bRP, bRM, bGP, bGM, bBP, bBM)
  }
}
case class DualGene(
  plus : String = "00000",
  minus : String = "00000" ) {
    lazy val gene0Serialized =
      plus + minus
    lazy val gene1Serialized =
      plus + "|" + minus
}
object DualGene {
  val parserGene0 = {
    val sectionParser = Parser.length(5)
    (sectionParser, sectionParser).mapN[Parser, DualGene](DualGene.apply)
  }
  val parserGene1 = {
    val sectionParser = Parser.until(Parser.char('|'))
    (sectionParser <* Parser.char('|'), sectionParser <* Parser.char('|').backtrack.orElse(Parser.unit)).mapN[Parser, DualGene](DualGene.apply)
  }
  def parse(groups : Iterator[String]) : DualGene = {
    val plus = groups.next()
    val minus = groups.next()
    DualGene(plus, minus)
  }
}
case class DomRecAllele(leftDom: Boolean, rightDom : Boolean) {
  lazy val serialized = {
    val l = if leftDom then 'A' else 'a'
    val r = if rightDom then 'A' else 'a'
    s"$l$r"
  }
}
object DomRecAllele {
  val parser = {
    val cParser = Parser.char('A').as(true).backtrack.orElse(Parser.char('a').as(false))
    (cParser, cParser).mapN[Parser, DomRecAllele](DomRecAllele.apply)
  }
  def parse(groups : Iterator[String]) : DomRecAllele = {
    val allele = groups.next()
    val domL = allele.charAt(0) == 'A'
    val domR = allele.charAt(1) == 'A'
    DomRecAllele(domL, domR)
  }

  val dom: DomRecAllele = DomRecAllele(true, true)
  val het: DomRecAllele = DomRecAllele(true, false)
  val sub: DomRecAllele = DomRecAllele(false, false)
}
case class DomRecGene(
  frontLeftLeg : DomRecAllele,
  frontRightLeg : DomRecAllele,
  backLeftLeg : DomRecAllele,
  backRightLeg : DomRecAllele,
  voicePitchHigh : DomRecAllele,
  voicePitchLow : DomRecAllele,
  voiceHoarse : DomRecAllele,
  smallPupils : DomRecAllele,
  eyelids : DomRecAllele,
  oblongEyes : DomRecAllele,
  multiPupils : DomRecAllele,
  teeth  : DomRecAllele,
  vMouth : DomRecAllele,
  openMouth : DomRecAllele,
  tiltedEars : DomRecAllele,
  nubTail : DomRecAllele,
  tailCurl : DomRecAllele,
  tailStiffness : DomRecAllele,
  stripePattern : DomRecAllele,
  splotchRepeatingPattern : DomRecAllele,
  noPattern : DomRecAllele,
  wings : DomRecAllele,
  wingIssues : DomRecAllele,
  missingWing : DomRecAllele,
  alignment : DomRecAllele,
  wingFeathers : DomRecAllele,
  longEyes : DomRecAllele,
  horizontalEyes : DomRecAllele,
  triangleEyes : DomRecAllele,
  missingPupilEyes : DomRecAllele,
  decorativeEyes : DomRecAllele,
  lashesEyes : DomRecAllele,
  spiralEyes : DomRecAllele,
  triangleEyes2 : DomRecAllele,
  geometricEyes : DomRecAllele,
  flatTail : DomRecAllele,
  bulbousTail : DomRecAllele,
  repeatedTail : DomRecAllele,
  thinTail : DomRecAllele,
  tail3d : DomRecAllele,
  noseExtrusion : DomRecAllele,
  noseStretch : DomRecAllele,
  noseFlat : DomRecAllele,
  noseRepeated : DomRecAllele,
  earFilled : DomRecAllele,
  earFlop : DomRecAllele,
  earSharp : DomRecAllele,
  earHalved : DomRecAllele,
  earConic : DomRecAllele,
  earCurlSynced : DomRecAllele,
  traditionalHorns : DomRecAllele,
  hornsCenter : DomRecAllele,
  hornsNone : DomRecAllele,
  hornsCurled : DomRecAllele,
  hornsNub : DomRecAllele,
  mouthHappiness : DomRecAllele,
  mouthCheeks : DomRecAllele,
  hornsThick : DomRecAllele,
  hornsThin : DomRecAllele,
  mouthMissingTeeth : DomRecAllele,
  mouthPointed : DomRecAllele,
  mouthCutoff : DomRecAllele,
  mouthWiggle : DomRecAllele) {
  lazy val serialized =
    frontLeftLeg.serialized + frontRightLeg.serialized +
    backLeftLeg.serialized + backRightLeg.serialized +
    voicePitchHigh.serialized +
    voicePitchLow.serialized + voiceHoarse.serialized +
    smallPupils.serialized + eyelids.serialized +
    oblongEyes.serialized + multiPupils.serialized +
    teeth.serialized + vMouth.serialized +
    openMouth.serialized + tiltedEars.serialized +
    nubTail.serialized + tailCurl.serialized +
    tailStiffness.serialized + stripePattern.serialized +
    splotchRepeatingPattern.serialized + noPattern.serialized +
    wings.serialized + wingIssues.serialized + missingWing.serialized +
    alignment.serialized + wingFeathers.serialized + longEyes.serialized +
    horizontalEyes.serialized + triangleEyes.serialized +
    missingPupilEyes.serialized + decorativeEyes.serialized +
    lashesEyes.serialized + spiralEyes.serialized +
    triangleEyes2.serialized + geometricEyes.serialized +
    flatTail.serialized + bulbousTail.serialized +
    repeatedTail.serialized + thinTail.serialized + tail3d.serialized +
    noseExtrusion.serialized + noseStretch.serialized + noseFlat.serialized +
    noseRepeated.serialized + earFilled.serialized + earFlop.serialized +
    earSharp.serialized + earHalved.serialized + earConic.serialized +
    earCurlSynced.serialized + traditionalHorns.serialized + hornsCenter.serialized +
    hornsNone.serialized + hornsCurled.serialized + hornsNub.serialized +
    mouthHappiness.serialized + mouthCheeks.serialized + hornsThick.serialized +
    hornsThin.serialized + mouthMissingTeeth.serialized + mouthPointed.serialized + mouthCutoff.serialized +
    mouthWiggle.serialized
}

object DomRecGene {
  @experimental
  val parser : Parser0[DomRecGene] = {
    FunctionHelper.curried(DomRecGene.apply).pure[Parser0]
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
      .ap(DomRecAllele.parser)
  }
  def parse(str: String) : DomRecGene = {
    val groups = str.grouped(2)
    val frontLeftLeg = DomRecAllele.parse(groups)
    val frontRightLeg = DomRecAllele.parse(groups)
    val backLeftLeg = DomRecAllele.parse(groups)
    val backRightLeg = DomRecAllele.parse(groups)
    val voicePitchHigh = DomRecAllele.parse(groups)
    val voicePitchLow = DomRecAllele.parse(groups)
    val voiceHoarse = DomRecAllele.parse(groups)
    val smallPupils = DomRecAllele.parse(groups)
    val eyelids = DomRecAllele.parse(groups)
    val oblongEyes = DomRecAllele.parse(groups)
    val multiPupils = DomRecAllele.parse(groups)
    val teeth = DomRecAllele.parse(groups)
    val vMouth = DomRecAllele.parse(groups)
    val openMouth = DomRecAllele.parse(groups)
    val tiltedEars = DomRecAllele.parse(groups)
    val nubTail = DomRecAllele.parse(groups)
    val tailCurl = DomRecAllele.parse(groups)
    val tailStiffness = DomRecAllele.parse(groups)
    val stripePattern = DomRecAllele.parse(groups)
    val splotchRepeatingPattern = DomRecAllele.parse(groups)
    val noPattern = DomRecAllele.parse(groups)
    val wings = DomRecAllele.parse(groups)
    val wingIssues = DomRecAllele.parse(groups)
    val missingWing = DomRecAllele.parse(groups)
    val alignment = DomRecAllele.parse(groups)
    val wingFeathers = DomRecAllele.parse(groups)
    val longEyes = DomRecAllele.parse(groups)
    val horizontalEyes = DomRecAllele.parse(groups)
    val triangleEyes = DomRecAllele.parse(groups)
    val missingPupilEyes = DomRecAllele.parse(groups)
    val decorativeEyes = DomRecAllele.parse(groups)
    val lashesEyes = DomRecAllele.parse(groups)
    val spiralEyes = DomRecAllele.parse(groups)
    val triangleEyes2 = DomRecAllele.parse(groups)
    val geometricEyes = DomRecAllele.parse(groups)
    val flatTail = DomRecAllele.parse(groups)
    val bulbousTail = DomRecAllele.parse(groups)
    val repeatedTail = DomRecAllele.parse(groups)
    val thinTail = DomRecAllele.parse(groups)
    val tail3d = DomRecAllele.parse(groups)
    val noseExtension = DomRecAllele.parse(groups)
    val noseStretch = DomRecAllele.parse(groups)
    val noseFlat = DomRecAllele.parse(groups)
    val noseRepeated = DomRecAllele.parse(groups)
    val earFilled = DomRecAllele.parse(groups)
    val earFlop = DomRecAllele.parse(groups)
    val earSharp = DomRecAllele.parse(groups)
    val earHalved = DomRecAllele.parse(groups)
    val earConic = DomRecAllele.parse(groups)
    val earCurlSynced = DomRecAllele.parse(groups)
    val traditionalHorns = DomRecAllele.parse(groups)
    val hornsCenter = DomRecAllele.parse(groups)
    val hornNone = DomRecAllele.parse(groups)
    val hornCurled = DomRecAllele.parse(groups)
    val hornNub = DomRecAllele.parse(groups)
    val mouthSmile = DomRecAllele.parse(groups)
    val mouthCheeks = DomRecAllele.parse(groups)
    val hornThick = DomRecAllele.parse(groups)
    val hornThin = DomRecAllele.parse(groups)
    val mouthMissingTeeth = DomRecAllele.parse(groups)
    val mouthPointed = DomRecAllele.parse(groups)
    val mouthCutoff = DomRecAllele.parse(groups)
    val mouthWiggle = DomRecAllele.parse(groups)

    DomRecGene(frontLeftLeg, frontRightLeg, backLeftLeg, backRightLeg, voicePitchHigh, voicePitchLow, voiceHoarse,
      smallPupils, eyelids, oblongEyes, multiPupils, teeth, vMouth, openMouth, tiltedEars, nubTail, tailCurl, tailStiffness,
      stripePattern, splotchRepeatingPattern, noPattern, wings, wingIssues, missingWing, alignment, wingFeathers, longEyes,
      horizontalEyes, triangleEyes, missingPupilEyes, decorativeEyes, lashesEyes, spiralEyes, triangleEyes2, geometricEyes,
      flatTail, bulbousTail, repeatedTail, thinTail, tail3d, noseExtension, noseStretch, noseFlat, noseRepeated, earFilled,
      earFlop, earSharp, earHalved, earConic, earCurlSynced, traditionalHorns, hornsCenter, hornNone, hornCurled, hornNub,
      mouthSmile, mouthCheeks, hornThick, hornThin, mouthMissingTeeth, mouthPointed, mouthCutoff, mouthWiggle)
  }
}
