import cats.parse.{Parser, Parser0}
import Parser0.given
import cats.syntax.all._
import net.bulbyvr.magic._
import scala.annotation.experimental
@experimental @main def hello: Unit =
  import scala.io.StdIn.readLine
  println("enter woofer code:")
  val code = readLine();
  val decryptCode = DogMath.unscramble(code)
  println(decryptCode)
  val rawdoggie = DogRegistry.importDog(code).get
  println(rawdoggie)
  val doggie = rawdoggie.toDog.get
  println(doggie.toString)
  println("\n")
  println(DogRegistry.exportDog(doggie.asRawDog))
@experimental
object DogMath: 
  val seperatorSymbol = '|'
  val tolerance = 0.0001f
  val maxGeneLen = 20
  val geneticEncodeSequenceLen = 20
  val separatorSequenceSymbols = List(':', ';', '<', '=', '>', '?', '@', '[', '#', ']', '^', '_', '*')
  private def getEncodedCharForSeparatorSequence(input: String) = 
    input match 
      case "|00" => DogMath.separatorSequenceSymbols(0)
      case "|01" => DogMath.separatorSequenceSymbols(1)
      case "|10" => DogMath.separatorSequenceSymbols(2)
      case "|11" => DogMath.separatorSequenceSymbols(3)
      case "||0" => DogMath.separatorSequenceSymbols(4)
      case "||1" => DogMath.separatorSequenceSymbols(5)
      case "|0|" => DogMath.separatorSequenceSymbols(6)
      case "|1|" => DogMath.separatorSequenceSymbols(7)
      case "|||" => DogMath.separatorSequenceSymbols(8)
      case "|0"  => DogMath.separatorSequenceSymbols(9)
      case "|1"  => DogMath.separatorSequenceSymbols(10)
      case "||"  => DogMath.separatorSequenceSymbols(11)
      case "|"   => DogMath.separatorSequenceSymbols(12)
      case _     => '|'
  private def getSeperatorSequenceForEncodedChar(input: Char) = 
    input match 
      case ':' => "|00"
      case ';' => "|01"
      case '<' => "|10"
      case '=' => "|11"
      case '>' => "||0"
      case '?' => "||1"
      case '@' => "|0|" 
      case '[' => "|1|"
      case '#' => "|||"
      case ']' => "|0"
      case '^' => "|1" 
      case '_' => "||"
      case '*' => "|"
      case _   => "ERROR"
  private def interiorScramble(s: String, i : Int) = 
    val num = s.charAt((i + 1) % s.length())
    val num2 = (i + num) % s.length 
    if num == 1 then
      // basically the same as num2 == (i + 1) % s.length 
      s 
    else
      val c = s.charAt(i)
      val c2 = s.charAt(num2)
      val strBuilder = StringBuilder(s)
      strBuilder.setCharAt(i, c2).setCharAt(num2, c).toString()
  def scramble(input: String) = 
    var text = input
    for (i <- 0 until input.length )
      text = interiorScramble(text, i)
    text
  def unscramble(input: String) =
    var text = input
    for (i <- input.length - 1 to 0 by -1)
      text = interiorScramble(text, i)
    text
  private def encodePrecedingZeros(input: String) =
    if input.length == 0 then 
      "" 
    else 
      if input.indexOf('1') != -1 then
        ""
      else 
        val stepSize = 25  
        val remainder = input.length % stepSize
        var text = (for (i <- input.length until 0 by -stepSize) yield 96 + stepSize).toList
        if remainder != 0 then 
          text :+= 96 + remainder 
        String.valueOf(text.toArray)
  private def encodeLookup(input: String) = 
    if input.length == 0 then 
      "" 
    else if input.indexOf('1') == -1 then 
      encodePrecedingZeros(input) 
    else 
      val (text, goodInput) = 
        if input.charAt(0) == '0' then 
          val num = input.indexOf('1')
          (encodePrecedingZeros(input.substring(0, num)), input.substring(num, input.length))
        else 
          ("", input)
      text + Integer.toHexString(Integer.parseInt(input, 2)).toUpperCase()
  private def decodeLookup(input: String) = 
    if input.length == 0 then 
      "" 
    else if input.length == 1 && input.charAt(0) >= 'a' && input.charAt(0) <= 'z' then 
      "0".repeat(input.charAt(0) - 97 + 1)
    else 
      Integer.toBinaryString(Integer.parseInt(input, 16))
  // forgive me : (
  def geneticEncode(input: String) = 
    var text = "" 
    var text2 = ""
    var i = 0
    while (i < input.length) {
      if input.charAt(i) == seperatorSymbol then 
        text += encodeLookup(text2) 
        val num = if i + 3 < input.length then 3 else (3 - (i + 3 - input.length))
        text += getEncodedCharForSeparatorSequence(input.substring(i, i + num))
        i += num 
        text2 = ""
      else
        if text2.length > 0 && !text2.contains("1") && input.charAt(i) == '1' then 
          text += encodePrecedingZeros(text2) 
          text2 = "" 
        text2 += input.charAt(i)
        if text2.length == geneticEncodeSequenceLen || i == input.length - 1 then 
          text += encodeLookup(text2)
          text2 = "" 
        else if text2.length > geneticEncodeSequenceLen then 
          println("Invalid SUB length")
        i += 1 
    }
    text
  def geneticDecode(input: String) = 
    val num = Math.floor(geneticEncodeSequenceLen / 4) 
    var text = "" 
    var text2 = "" 
    for (i <- 0 until input.length) {
      if separatorSequenceSymbols.contains(input.charAt(i)) then 
        text += decodeLookup(text2) 
        text += getSeperatorSequenceForEncodedChar(input.charAt(i))
        text2 = ""
      else if input.charAt(i) >= 'a' && input.charAt(i) <= 'z' then 
        text += decodeLookup(text2) 
        text += decodeLookup(input.charAt(i).toString())
        text2 = ""
      else 
        text2 += input.charAt(i)
        if text2.length == num || i == input.length - 1 then 
          text += decodeLookup(text2) 
          text2 = "" 
        else if (text2.length > num) then 
          println ("Invalid SUB length")
    }
    text
@experimental
object DogRegistry: 
  val currentGeneVersion = 3
  val dogExportSeperator = "^"
  def exportDog(rawDog : RawDog) : String = 
    val dogGene = DogMath.geneticEncode(rawDog.dogGene)
    val domRecGene = DogMath.geneticEncode(rawDog.domRecGene.replace('A','1').replace('a','0'))
    DogMath.scramble("^^^" + rawDog.geneVersion.versionId + dogExportSeperator + 
    dogGene + dogExportSeperator + domRecGene + dogExportSeperator + rawDog.dogAge.toString.toUpperCase + 
    dogExportSeperator + rawDog.dogAgeProgress + dogExportSeperator + rawDog.eolModifier + 
    dogExportSeperator + rawDog.lifeExtension + dogExportSeperator + rawDog.personality.serialized 
    + dogExportSeperator + rawDog.dogName)
  def importDog(dogCode : String): Option[RawDog] = 
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
        ,sectionParser.map(DogMath.geneticDecode(_))
        ,sectionParser.map(DogMath.geneticDecode(_)).map(_.replace('0', 'a').replace('1', 'A'))
        ,sectionParser.map(DogAge.parseString(_))
        ,sectionParser.mapFilter(_.toFloatOption)
        ,flag2thingies
        ,flag2thingies
        ,if flag then DogPersonality.parser <* Parser.char('^') else Parser.pure(DogPersonality.empty)
        , Parser.until(Parser.end)
        ).mapN(RawDog.apply)
    }
    parser.parseAll(text).toOption
        
enum GeneVersion(val versionId: Int, val name: String):
  case All extends GeneVersion(-1, "ALL")
  case Zero extends GeneVersion(0, "ZERO")
  case One extends GeneVersion(1, "ONE")
  case Two extends GeneVersion(2, "TWO")
  case Three extends GeneVersion(3, "THREE")
  
object GeneVersion:
  val parser = 
    Parser.until(Parser.char('^')).mapFilter(_.toIntOption).mapFilter(ofInt(_))
  def ofInt(i: Int) = 
    i match 
      case -1 => Some(All)
      case 0 => Some(Zero)
      case 1 => Some(One)
      case 2 => Some(Two) 
      case 3 => Some(Three)
      case _ => None
  def parseString(str: String): Option[GeneVersion] = 
    str.toIntOption match 
      case Some(i) => ofInt(i)
      case None => 
        str match 
          case "ALL" => Some(All)
          case "ZERO" => Some(Zero)
          case "ONE" => Some(One)
          case "TWO" => Some(Two)
          case "THREE" => Some(Three)
          case _ => None

enum EnergyPersonality(id: Int): 
  case Goof extends EnergyPersonality(0)
  case StandardEnergy extends EnergyPersonality(1)
  case Layabout extends EnergyPersonality(2)
object EnergyPersonality: 
  def parseString(str: String) = 
    EnergyPersonality.fromOrdinal(str.toInt)
enum LoudnessPersonality(id: Int): 
  case StandardLoud extends LoudnessPersonality(0)
  case Loud extends LoudnessPersonality(1)
  case Quiet extends LoudnessPersonality(2)
object LoudnessPersonality: 
  def parseString(str: String) = 
    LoudnessPersonality.fromOrdinal(str.toInt)
enum MischiefPersonality(id: Int): 
  case Polite extends MischiefPersonality(0)
  case StandardMischief extends MischiefPersonality(1)
  case Mischevious extends MischiefPersonality(2)
object MischiefPersonality: 
  def parseString(str: String) = 
    MischiefPersonality.fromOrdinal(str.toInt)
enum PettablePersonality(id: Int): 
  case LikesPetting extends PettablePersonality(0)
  case DislikesPetting extends PettablePersonality(1)
object PettablePersonality: 
  def parseString(str: String) = 
    PettablePersonality.fromOrdinal(str.toInt)
enum FoodPersonality(id: Int): 
  case Glutton extends FoodPersonality(0)
  case Standard extends FoodPersonality(1)
  case FoodAverse extends FoodPersonality(2)
object FoodPersonality: 
  def parseString(str: String) = 
    FoodPersonality.fromOrdinal(str.toInt)
enum NicenessPersonality: 
  case Nice 
  case NiceStandard 
  case Mean 
object NicenessPersonality: 
  def parseString(str: String) = 
    NicenessPersonality.fromOrdinal(str.toInt)
enum SocialPersonality: 
  case Social 
  case SocialStandard 
  case Aloof
object SocialPersonality: 
  def parseString(str: String) = 
    SocialPersonality.fromOrdinal(str.toInt)
enum DogAge: 
  case Empty 
  case Puppy
  case Child
  case Teen 
  case YoungAdult
  case Adult
  case Ancient
object DogAge: 
  def parseString(str: String) = 
    str.toIntOption match 
      case Some(i) => DogAge.fromOrdinal(i)
      case None => 
        str match 
          case "NONE" => Empty 
          case "PUPPY" => Puppy 
          case "CHILD" => Child 
          case "TEEN" => Teen 
          case "YOUNG_ADULT" => YoungAdult 
          case "ADULT" => Adult 
          case "ANCIENT" => Ancient 
          case _ => throw IllegalArgumentException("invalid life stage")
case class DogPersonality(social: SocialPersonality, energy: EnergyPersonality, food: FoodPersonality, mischief: MischiefPersonality, niceness: NicenessPersonality,  pettable: PettablePersonality, loudness: LoudnessPersonality): 
  lazy val serialized = 
    social.ordinal.toString + energy.ordinal.toString + food.ordinal.toString + mischief.ordinal.toString + niceness.ordinal.toString + pettable.ordinal.toString + 
    loudness.ordinal.toString
object DogPersonality {
  val parser = 
    (Parser.length(1).map(SocialPersonality.parseString(_))
    ,Parser.length(1).map(EnergyPersonality.parseString(_))
    ,Parser.length(1).map(FoodPersonality.parseString(_))
    ,Parser.length(1).map(MischiefPersonality.parseString(_))
    ,Parser.length(1).map(NicenessPersonality.parseString(_))
    ,Parser.length(1).map(PettablePersonality.parseString(_))
    ,Parser.char('^').as(LoudnessPersonality.StandardLoud).backtrack.orElse(Parser.length(1).map(LoudnessPersonality.parseString(_)))
    ).mapN(DogPersonality.apply)
  val empty = 
    DogPersonality(SocialPersonality.SocialStandard, EnergyPersonality.StandardEnergy, FoodPersonality.Standard, MischiefPersonality.StandardMischief, NicenessPersonality.NiceStandard,
    PettablePersonality.LikesPetting, LoudnessPersonality.StandardLoud)
}
// Not what you think you sicko!!!
@experimental
case class RawDog(geneVersion: GeneVersion, dogGene: String, domRecGene: String, dogAge: DogAge, dogAgeProgress: Float, eolModifier: Float, lifeExtension: Float, personality: DogPersonality, dogName : String):
  def toDog : Option[Dog] = 
    val gene0res = DogGene0.parser.parse(dogGene)
    gene0res match 
      case Left(e) =>
        println(e)
        None
      case Right((rest, prop0genes)) =>
        println(rest)
        DogGene1.parser.parseAll(rest) match 
          case Left(e) => 
            println(e)
            None 
          case Right(prop1genes) => 
            val domRecGenes = DomRecGene.parser.parseAll(domRecGene).toOption.get
            Some(Dog(personality, prop0genes, prop1genes, domRecGenes, dogAge, dogAgeProgress, eolModifier, lifeExtension, dogName))
@experimental
case class Dog(personality: DogPersonality, geneProp0 : DogGene0, geneProp1: DogGene1, domRecGene : DomRecGene, age: DogAge, ageProgress: Float, 
  eolModifier: Float, lifeExtension: Float, dogName : String):
  def asRawDog = 
    val dogGene = geneProp0.serialized + "|" + geneProp1.serialized 
    RawDog(GeneVersion.Three, dogGene, domRecGene.serialized, age, ageProgress, eolModifier, lifeExtension, personality, dogName)
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
      ): 
    lazy val serialized = 
      randomSeed + bodyShiny.serialized + bodyColor.serialized +
      legShiny.serialized + legColor.serialized + noseShiny.serialized + 
      noseColor.serialized + noseModA.gene0Serialized + hornSize.gene0Serialized + 
      earModA.gene0Serialized + earCurl.gene0Serialized + snoutModA.gene0Serialized +
      snoutModB.gene0Serialized + snoutModC.gene0Serialized + headSize.gene0Serialized + 
      wingSize.gene0Serialized + stanceWidthFront.gene0Serialized + stanceWidthBack.gene0Serialized +
      patternColor.patternSerialized + patternAlpha + patternShiny.serialized + patternNum + 
      patternFlipX + patternFlipY + patternInfo + extraBits
@experimental 
object DogGene0:
  import net.bulbyvr.magic.FunctionHelper
  val parser : Parser0[DogGene0] = 
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
  ): 
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
object DogGene1: 
  def parser = 
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
  def parse(groups : Iterator[String]) =
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
case class ShinyGene( 
    metallicPlus : String, 
    metallicMinus : String,
    glossPlus : String,
    glossMinus : String): 
  lazy val serialized = 
    metallicPlus + metallicMinus + 
    glossPlus + glossMinus
object ShinyGene:
  val parser = 
    val sectionParser = Parser.length(5)
    (sectionParser, sectionParser, sectionParser, sectionParser).mapN[Parser, ShinyGene](ShinyGene.apply)
case class ColorGene(
    emissionRedPlus : String, 
    emissionRedMinus : String,
    emissionGreenPlus : String, 
    emissionGreenMinus : String, 
    emissionBluePlus : String,
    emissionBlueMinus : String,
    baseRedPlus : String,
    baseRedMinus : String, 
    baseGreenPlus : String, 
    baseGreenMinus : String,
    baseBluePlus : String,
    baseBlueMinus : String
  ):
  lazy val serialized = 
    emissionRedPlus + emissionRedMinus + emissionGreenPlus + 
    emissionGreenMinus + emissionBluePlus + emissionBlueMinus + 
    baseRedPlus + baseRedMinus + baseGreenPlus + 
    baseGreenMinus + baseBluePlus + baseBlueMinus 
  lazy val patternSerialized =
    baseRedPlus + baseRedMinus + baseGreenPlus + 
    baseGreenMinus + baseBluePlus + baseBlueMinus +
    emissionRedPlus + emissionRedMinus + emissionGreenPlus + 
    emissionGreenMinus + emissionBluePlus + emissionBlueMinus
object ColorGene:
  private val parserBase = 
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
  val parser : Parser[ColorGene] = 
    parserBase.mapN[Parser, ColorGene](ColorGene.apply)
  val parserPattern = 
    parserBase.mapN[Parser, ColorGene]( ( g, h, i, j, k, l, a, b, c, d, e, f) => 
        ColorGene(a, b, c, d, e, f, g, h, i, j, k, l) )
  def parsePattern(groups : Iterator[String]) : ColorGene = 
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
  def parse(groups : Iterator[String]) : ColorGene = 
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
case class DualGene(
  plus : String,
  minus : String ):
    lazy val gene0Serialized = 
      plus + minus 
    lazy val gene1Serialized = 
      plus + "|" + minus 
object DualGene:
  val parserGene0 = 
    val sectionParser = Parser.length(5)
    (sectionParser, sectionParser).mapN[Parser, DualGene](DualGene.apply)
  val parserGene1 = 
    val sectionParser = Parser.until(Parser.char('|'))
    (sectionParser <* Parser.char('|'), sectionParser <* Parser.char('|').backtrack.orElse(Parser.unit)).mapN[Parser, DualGene](DualGene.apply)
  def parse(groups : Iterator[String]) : DualGene = 
    val plus = groups.next()
    val minus = groups.next()
    DualGene(plus, minus)
case class DomRecAllele(leftDom: Boolean, rightDom : Boolean): 
  lazy val serialized = 
    val l = if leftDom then 'A' else 'a'
    val r = if rightDom then 'A' else 'a'
    s"$l$r"
object DomRecAllele: 
  val parser = 
    val cParser = Parser.char('A').as(true).backtrack.orElse(Parser.char('a').as(false))
    (cParser, cParser).mapN[Parser, DomRecAllele](DomRecAllele.apply)
  def parse(groups : Iterator[String]) : DomRecAllele = 
    val allele = groups.next() 
    val domL = allele.charAt(0) == 'A'
    val domR = allele.charAt(1) == 'A'
    DomRecAllele(domL, domR)
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
  mouthWiggle : DomRecAllele):
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
@experimental
object DomRecGene:
  val parser : Parser0[DomRecGene] =
    import net.bulbyvr.magic.FunctionHelper
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
  def parse(str: String) : DomRecGene = 
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
