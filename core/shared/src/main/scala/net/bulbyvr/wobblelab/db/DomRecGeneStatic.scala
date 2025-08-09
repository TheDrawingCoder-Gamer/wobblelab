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
  val namedValues: List[(String, DomRecGeneStatic)] = List(
    "Front Left Leg" -> recessiveOnly(TraitType.Dom, MissingFrontLeftLeg, GeneVersion.Zero),
    "Front Right Leg" -> recessiveOnly(TraitType.Dom, MissingFrontRightLeg, GeneVersion.Zero),
    "Back Left Leg" -> recessiveOnly(TraitType.Dom, MissingBackLeftLeg, GeneVersion.Zero),
    "Back Right Leg" -> recessiveOnly(TraitType.Dom, MissingBackRightLeg, GeneVersion.Zero),
    "High Voice Pitch" -> recessiveOnly(TraitType.Het, VoicePitchHigh, GeneVersion.Zero),
    "Low Voice Pitch" -> recessiveOnly(TraitType.Het, VoicePitchLow, GeneVersion.Zero),
    "Hoarse Voice" -> recessiveOnly(TraitType.Dom, VoiceHoarse, GeneVersion.Zero),
    "Small Pupils" -> recessiveOnly(TraitType.Dom, SmallPupils, GeneVersion.Zero),
    "Eyelids" -> recessiveOnly(TraitType.Het, Eyelids, GeneVersion.Zero),
    "Oblong Eyes" -> recessiveOnly(TraitType.Het, OblongEyes, GeneVersion.Zero),
    "Multi Pupils" -> recessiveOnly(TraitType.Dom, MultiPupils, GeneVersion.Zero),
    "Teeth" -> recessiveOnly(TraitType.Sub, Teeth, GeneVersion.Zero),
    "V Mouth" -> recessiveOnly(TraitType.Dom, VMouth, GeneVersion.Zero),
    "Open Mouth" -> recessiveOnly(TraitType.Dom, OpenMouth, GeneVersion.Zero),
    "Tilted Ears" -> DomRecGeneStatic(GeneVersion.Zero, None, TiltedEars, None, TraitType.Dom),
    "Tail" -> DomRecGeneStatic(GeneVersion.Zero, NoTail, NubTail, None, TraitType.Dom),
    "Tail Curl" -> DomRecGeneStatic(GeneVersion.Zero, None, SlightlyCurledTail, CurledTail, TraitType.Dom),
    "Tail Stiffness" -> inverseRecessive(GeneVersion.Zero, StiffTail),
    "Stripe Pattern" -> recessiveOnly(TraitType.Dom, StripePattern, GeneVersion.Zero),
    "Splotch/Repeating Pattern" -> DomRecGeneStatic(GeneVersion.Zero, None, SplotchPattern, RepeatingPattern, TraitType.Dom),
    "Pattern" -> DomRecGeneStatic(GeneVersion.Zero, NoPattern, None, None, TraitType.Dom),
    "Wings" -> inverseRecessive(GeneVersion.Two, NoWings),
    "Wing Issues" -> recessiveOnly(TraitType.Dom, WingIssues, GeneVersion.Two),
    "Missing Wings" -> DomRecGeneStatic(GeneVersion.Two, MissingLeftWing, None, MissingRightWing, TraitType.Het),
    "Wing Alignment" -> DomRecGeneStatic(GeneVersion.Two, AlignmentGood, AlignmentNeutral, AlignmentEvil, TraitType.Het),
    "Wing Feathers" -> recessiveOnly(TraitType.Dom, WingFeathers, GeneVersion.Two),
    "Long Eyes" -> recessiveOnly(TraitType.Dom, LongEyes, GeneVersion.One),
    "Horizontal Eyes" -> recessiveOnly(TraitType.Het, HorizontalEyes, GeneVersion.One),
    "Triangle Eyes" -> recessiveOnly(TraitType.Dom, TriangleEyes, GeneVersion.One),
    "Missing Pupil Eyes" -> recessiveOnly(TraitType.Dom, MissingPupilEyes, GeneVersion.One),
    "Decorative Eyes" -> recessiveOnly(TraitType.Dom, DecorativeEyes, GeneVersion.One),
    "Eyelashes" -> recessiveOnly(TraitType.Dom, LashesEyes, GeneVersion.One),
    "Spiral Eyes" -> recessiveOnly(TraitType.Dom, SpiralEyes, GeneVersion.Two),
    // duplicate? its in the file so :shrug:
    "Triangle Eyes (again)" -> recessiveOnly(TraitType.Dom, TriangleEyes, GeneVersion.Two),
    "Geometric Eyes" -> recessiveOnly(TraitType.Het, GeometricEyes, GeneVersion.Two),
    "Flat Tail" -> recessiveOnly(TraitType.Dom, FlatTail, GeneVersion.Two),
    "Bulbous Tail" -> recessiveOnly(TraitType.Dom, BulbousTail, GeneVersion.Two),
    "Repeated Tail" -> recessiveOnly(TraitType.Het, RepeatedTail, GeneVersion.Two),
    "Thin Tail" -> recessiveOnly(TraitType.Dom, ThinTail, GeneVersion.Two),
    "3D Tail" -> recessiveOnly(TraitType.Het, Tail3D, GeneVersion.Two),
    "Nose Extrusion" -> DomRecGeneStatic(GeneVersion.Two, None, NoseExtrusion, None, TraitType.Dom),
    "Nose Squish/Stretch" -> DomRecGeneStatic(GeneVersion.Two, NoseSquish, None, NoseStretch, TraitType.Het),
    "Flat Nose" -> recessiveOnly(TraitType.Dom, NoseFlat, GeneVersion.Two),
    "Repeated Nose" -> recessiveOnly(TraitType.Dom, NoseRepeated, GeneVersion.Two),
    "Filled Ear" -> recessiveOnly(TraitType.Sub, EarFilled, GeneVersion.Two),
    "Ear Floppiness" -> DomRecGeneStatic(GeneVersion.Two, None, EarFloppy, EarPartialFlop, TraitType.Dom),
    "Sharp Ear" -> recessiveOnly(TraitType.Dom, EarSharp, GeneVersion.Two),
    "Halved Ear" -> DomRecGeneStatic(GeneVersion.Two, None, EarHalved, None, TraitType.Dom),
    "Conic Ear" -> recessiveOnly(TraitType.Dom, EarConic, GeneVersion.Two),
    "Ear Curl Sync" -> inverseRecessive(GeneVersion.Two, EarCurlSynced),
    "Traditional Horns" -> DomRecGeneStatic(GeneVersion.Two, None, HornsTraditional, HornsTraditional, TraitType.Dom),
    "Center Horns" -> recessiveOnly(TraitType.Dom, HornsCenter, GeneVersion.Two),
    "Horns" -> inverseRecessive(GeneVersion.Two, HornsNone),
    "Curled Horns" -> recessiveOnly(TraitType.Dom, HornsCurled, GeneVersion.Two),
    "Nub Horns" -> recessiveOnly(TraitType.Dom, HornsNub, GeneVersion.Two),
    "Mouth Emotion" -> DomRecGeneStatic(GeneVersion.Two, MouthSmile, MouthNeutral, MouthFrown, TraitType.Het),
    "Cheeky Mouth" -> recessiveOnly(TraitType.Dom, MouthCheeks, GeneVersion.Two),
    "Thick Horns" -> recessiveOnly(TraitType.Dom, HornsThick, GeneVersion.Two),
    "Thin Horns" -> recessiveOnly(TraitType.Dom, HornsThin, GeneVersion.Two),
    "Mouth Missing Teeth" -> recessiveOnly(TraitType.Dom, MouthMissingTeeth, GeneVersion.Two),
    "Pointed Mouth" -> recessiveOnly(TraitType.Dom, MouthPointed, GeneVersion.Two),
    "Cutoff Mouth" -> recessiveOnly(TraitType.Dom, MouthCutoff, GeneVersion.Two),
    "Wiggly Mouth" -> recessiveOnly(TraitType.Dom, MouthWiggle, GeneVersion.Two)
  )
  
  val values: List[DomRecGeneStatic] = namedValues.map(_._2)
}
