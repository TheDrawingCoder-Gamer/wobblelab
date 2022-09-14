import cats.parse.{Parser, Parser0}
import Parser0.given
import cats.syntax.all._
@main def hello: Unit =
  import scala.io.StdIn.readLine
  println("enter woofer code:")
  val code = readLine();
  val decryptCode = DogMath.unscramble(code)
  println(decryptCode)
  println(DogRegistry.importDog(code).get.toDog.toString)
def msg = "I was compiled by Scala 3. :)"

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
object DogRegistry: 
  val currentGeneVersion = 3
  val dogExportSeperator = "^"
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
        ).mapN(RawDog.apply)
    }
    parser.parseAll(text).toOption
        
enum GeneVersion(id: Int, name: String):
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
case class DogPersonality(social: SocialPersonality, energy: EnergyPersonality, food: FoodPersonality, mischief: MischiefPersonality, niceness: NicenessPersonality,  pettable: PettablePersonality, loudness: LoudnessPersonality)
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
case class RawDog(geneVersion: GeneVersion, dogGene: String, domRecGene: String, dogAge: DogAge, dogAgeProgress: Float, eolModifier: Float, lifeExtension: Float, personality: DogPersonality):
  def toDog = 
    val genes = dogGene.split('|')
    val gene0Pre = genes(0).grouped(5)
    println(gene0Pre.length)
    val prop0genes = DogGene0.parse(genes(0).grouped(5))
    val prop1genes = DogGene1.parse(genes.tail.iterator)
    val domRecGenes = DomRecGene.parse(domRecGene)
    Dog(personality, prop0genes, prop1genes, domRecGenes, dogAge, dogAgeProgress, eolModifier, lifeExtension)
case class Dog(personality: DogPersonality, geneProp0 : DogGene0, geneProp1: DogGene1, domRecGene : DomRecGene, age: DogAge, ageProgress: Float, eolModifier: Float, lifeExtension: Float)
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
      )
object DogGene0: 
  def parse(groups: Iterator[String]) : DogGene0 =
    val randomSeed = List.fill(2)(groups.next).fold("")(_ + _)
    val bodyShiny = ShinyGene.parse(groups)
    val bodyColor = ColorGene.parse(groups)
    val legShiny = ShinyGene.parse(groups)
    val legColor = ColorGene.parse(groups)
    val noseShiny = ShinyGene.parse(groups)
    val noseColor = ColorGene.parse(groups)
    val noseModA = DualGene.parse(groups)
    val hornSize = DualGene.parse(groups)
    val earModA = DualGene.parse(groups)
    val earCurl = DualGene.parse(groups)
    val snoutModA = DualGene.parse(groups)
    val snoutModB = DualGene.parse(groups)
    val snoutModC = DualGene.parse(groups) 
    val headSize = DualGene.parse(groups)
    val wingSize = DualGene.parse(groups)
    val stanceWidthFront = DualGene.parse(groups)
    val stanceWidthBack = DualGene.parse(groups)
    val patternColor = ColorGene.parsePattern(groups)
    val patternAlpha = groups.next()
    val patternShiny = ShinyGene.parse(groups)
    val patternNum = groups.next()
    val patternFlipX = List.fill(5)(Option.when(groups.hasNext)(groups.next)).flatten.fold("")(_ + _)
    val patternFlipY = List.fill(5)(Option.when(groups.hasNext)(groups.next)).flatten.fold("")(_ + _)
    val patternInfo = List.fill(25)(Option.when(groups.hasNext)(groups.next)).flatten.fold("")(_ + _)
    val extraBits = groups.toList.fold("")(_ + _)
    DogGene0(randomSeed, bodyShiny, bodyColor, legShiny, legColor, noseShiny, noseColor, noseModA, hornSize, earModA, earCurl,
      snoutModA, snoutModB, snoutModC, headSize, wingSize, stanceWidthFront, stanceWidthBack, patternColor, patternAlpha, patternShiny, 
      patternNum, patternFlipX, patternFlipY, patternInfo, extraBits)

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
  )
object DogGene1: 
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
    glossPlus : String,
    glossMinus : String,
    metallicPlus : String, 
    metallicMinus : String)
object ShinyGene: 
  def parse(groups: Iterator[String]): ShinyGene =
    val metallicPlus = groups.next()
    val metallicMinus = groups.next() 
    val glossPlus = groups.next() 
    val glossMinus = groups.next()
    ShinyGene(glossPlus, glossMinus, metallicPlus, metallicMinus)
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
  )
object ColorGene: 
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
  minus : String )
object DualGene: 
  def parse(groups : Iterator[String]) : DualGene = 
    val plus = groups.next()
    val minus = groups.next()
    DualGene(plus, minus)
case class DomRecAllele(leftDom: Boolean, rightDom : Boolean)
object DomRecAllele: 
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
  mouthWiggle : DomRecAllele)
object DomRecGene:
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
