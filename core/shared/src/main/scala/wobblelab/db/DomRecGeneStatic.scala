package net.bulbyvr
package wobblelab
package db

import io.circe.*
import io.circe.Decoder.Result
import io.circe.syntax.*

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
    recessiveOnly(TraitType.Dom, MouthMissingTeeth, GeneVersion.Two)

  )
}

given domRecGeneDecoder: Decoder[DomRecGeneStatic]
  = Decoder.forProduct5("version", "AA", "Aa", "aa", "defaultValue")(DomRecGeneStatic.apply)
given domRecGeneEncoder: Encoder[DomRecGeneStatic] = {
  Encoder.forProduct5("version", "AA", "Aa", "aa", "defaultValue") { g =>
    (g.version, g.dom, g.het, g.sub, g.defaultValue)
  }
}
