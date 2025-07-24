package net.bulbyvr
package wobblelab
package db

import io.circe.*
import io.circe.Decoder.Result
import io.circe.syntax.*
import net.bulbyvr

case class DomRecGeneStatic(
                     version: GeneVersion,
                     dom: DomRecGeneProperty,
                     het: DomRecGeneProperty,
                     sub: DomRecGeneProperty,
                     defaultValue: TraitType
                     ) {
  def apply(): DomRecGene = DomRecGene(this, defaultValue)
  def apply(value: TraitType): DomRecGene = DomRecGene(this, value)
}

object DomRecGeneStatic {
  private def recessiveOnly(default: TraitType, v: DomRecGeneProperty, version: GeneVersion): DomRecGeneStatic =
    DomRecGeneStatic(version, DomRecGeneProperty.None, DomRecGeneProperty.None, v, default)
  private def inverseRecessive(version: GeneVersion, v: DomRecGeneProperty, default: TraitType = TraitType.Dom): DomRecGeneStatic =
    DomRecGeneStatic(version, v, v, DomRecGeneProperty.None, default)

  object Indices:
    val noWings: Int = 21
    val alignment: Int = 24
    val wingFeathers: Int = 25

    val hornsNub: Int = 54
    val hornsThick: Int = 57
    val hornsThin: Int = 58
    val hornsCurled: Int = 53
    val noHorns: Int = 52
    
    val noseFlat: Int = 42
    val noseRepeated: Int = 43
    val noseExtrusion: Int = 40
    val noseSquishStretch: Int = 41
    
    val noNubTail: Int = 15
    val tailCurl: Int = 16
    val stiffTail: Int = 17
    val flatTail: Int = 35
    val bulbousTail: Int = 36
    val repeatedTail: Int = 37
    val thinTail: Int = 38
    val tail3D: Int = 39
    
    val traditionalHorns: Int = 50
    val centerHorns: Int = 51
    
    val teeth: Int = 11
    val vMouth: Int = 12
    val openMouth: Int = 13
    val mouthEmotion: Int = 55
    val mouthCheeks: Int = 56
    val mouthMissingTeeth: Int = 59
    val mouthPointed: Int = 60
    val mouthCutoff = 61
    val mouthWiggle = 62
    
    val tiltedEars: Int = 14
    val earSharp: Int = 46
    val earFilled: Int = 44
    val earFloppiness: Int = 45
    val earHalved: Int = 47
    val earConic: Int = 48
    val earSynced: Int = 49

    val smallPupils: Int = 7
    val eyelids: Int = 8
    val oblongEyes: Int = 9
    val multiPupils: Int = 10
    val longEyes: Int = 26
    val horizontalEyes: Int = 27
    val triangleEyes: Int = 28
    val missingPupilsEyes: Int = 29
    val decorativeEyes: Int = 30
    val lashesEyes: Int = 31
    val spiralEyes: Int = 32
    val triangleEyes2: Int = 33
    val geometricEyes: Int = 34

  import DomRecGeneProperty.*
  val values: List[DomRecGeneStatic] = List(
    recessiveOnly(TraitType.Dom, MissingFrontLeftLeg, GeneVersion.Zero),
    recessiveOnly(TraitType.Dom, MissingFrontRightLeg, GeneVersion.Zero),
    recessiveOnly(TraitType.Dom, MissingBackLeftLeg, GeneVersion.Zero),
    recessiveOnly(TraitType.Dom, MissingBackRightLeg, GeneVersion.Zero),
    recessiveOnly(TraitType.Het, VoicePitchHigh, GeneVersion.Zero),
    recessiveOnly(TraitType.Het, VoicePitchLow, GeneVersion.Zero),
    recessiveOnly(TraitType.Dom, VoiceHoarse, GeneVersion.Zero),
    recessiveOnly(TraitType.Dom, SmallPupils, GeneVersion.Zero),
    recessiveOnly(TraitType.Het, Eyelids, GeneVersion.Zero),
    recessiveOnly(TraitType.Het, OblongEyes, GeneVersion.Zero),
    recessiveOnly(TraitType.Dom, MultiPupils, GeneVersion.Zero),
    recessiveOnly(TraitType.Sub, Teeth, GeneVersion.Zero),
    recessiveOnly(TraitType.Dom, VMouth, GeneVersion.Zero),
    recessiveOnly(TraitType.Dom, OpenMouth, GeneVersion.Zero),
    DomRecGeneStatic(GeneVersion.Zero, None, TiltedEars, None, TraitType.Dom),
    DomRecGeneStatic(GeneVersion.Zero, NoTail, NubTail, None, TraitType.Dom),
    DomRecGeneStatic(GeneVersion.Zero, None, SlightlyCurledTail, CurledTail, TraitType.Dom),
    inverseRecessive(GeneVersion.Zero, StiffTail),
    recessiveOnly(TraitType.Dom, StripePattern, GeneVersion.Zero),
    DomRecGeneStatic(GeneVersion.Zero, None, SplotchPattern, RepeatingPattern, TraitType.Dom),
    DomRecGeneStatic(GeneVersion.Zero, NoPattern, None, None, TraitType.Dom),
    inverseRecessive(GeneVersion.Two, NoWings),
    recessiveOnly(TraitType.Dom, WingIssues, GeneVersion.Two),
    DomRecGeneStatic(GeneVersion.Two, MissingLeftWing, None, MissingRightWing, TraitType.Het),
    DomRecGeneStatic(GeneVersion.Two, AlignmentGood, AlignmentNeutral, AlignmentEvil, TraitType.Het),
    recessiveOnly(TraitType.Dom, WingFeathers, GeneVersion.Two),
    recessiveOnly(TraitType.Dom, LongEyes, GeneVersion.One),
    recessiveOnly(TraitType.Het, HorizontalEyes, GeneVersion.One),
    recessiveOnly(TraitType.Dom, TriangleEyes, GeneVersion.One),
    recessiveOnly(TraitType.Dom, MissingPupilEyes, GeneVersion.One),
    recessiveOnly(TraitType.Dom, DecorativeEyes, GeneVersion.One),
    recessiveOnly(TraitType.Dom, LashesEyes, GeneVersion.One),
    recessiveOnly(TraitType.Dom, SpiralEyes, GeneVersion.Two),
    // duplicate? its in the file so :shrug:
    recessiveOnly(TraitType.Dom, TriangleEyes, GeneVersion.Two),
    recessiveOnly(TraitType.Het, GeometricEyes, GeneVersion.Two),
    recessiveOnly(TraitType.Dom, FlatTail, GeneVersion.Two),
    recessiveOnly(TraitType.Dom, BulbousTail, GeneVersion.Two),
    recessiveOnly(TraitType.Het, RepeatedTail, GeneVersion.Two),
    recessiveOnly(TraitType.Dom, ThinTail, GeneVersion.Two),
    recessiveOnly(TraitType.Het, Tail3D, GeneVersion.Two),
    DomRecGeneStatic(GeneVersion.Two, None, NoseExtrusion, None, TraitType.Dom),
    DomRecGeneStatic(GeneVersion.Two, NoseSquish, None, NoseStretch, TraitType.Het),
    recessiveOnly(TraitType.Dom, NoseFlat, GeneVersion.Two),
    recessiveOnly(TraitType.Dom, NoseRepeated, GeneVersion.Two),
    recessiveOnly(TraitType.Sub, EarFilled, GeneVersion.Two),
    DomRecGeneStatic(GeneVersion.Two, None, EarFloppy, EarPartialFlop, TraitType.Dom),
    recessiveOnly(TraitType.Dom, EarSharp, GeneVersion.Two),
    DomRecGeneStatic(GeneVersion.Two, None, EarHalved, None, TraitType.Dom),
    recessiveOnly(TraitType.Dom, EarConic, GeneVersion.Two),
    inverseRecessive(GeneVersion.Two, EarCurlSynced),
    DomRecGeneStatic(GeneVersion.Two, None, HornsTraditional, HornsTraditional, TraitType.Dom),
    recessiveOnly(TraitType.Dom, HornsCenter, GeneVersion.Two),
    inverseRecessive(GeneVersion.Two, HornsNone),
    recessiveOnly(TraitType.Dom, HornsCurled, GeneVersion.Two),
    recessiveOnly(TraitType.Dom, HornsNub, GeneVersion.Two),
    DomRecGeneStatic(GeneVersion.Two, MouthSmile, MouthNeutral, MouthFrown, TraitType.Het),
    recessiveOnly(TraitType.Dom, MouthCheeks, GeneVersion.Two),
    recessiveOnly(TraitType.Dom, HornsThick, GeneVersion.Two),
    recessiveOnly(TraitType.Dom, HornsThin, GeneVersion.Two),
    recessiveOnly(TraitType.Dom, MouthMissingTeeth, GeneVersion.Two),
    recessiveOnly(TraitType.Dom, MouthPointed, GeneVersion.Two),
    recessiveOnly(TraitType.Dom, MouthCutoff, GeneVersion.Two),
    recessiveOnly(TraitType.Dom, MouthWiggle, GeneVersion.Two)
  )
}
