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
import net.bulbyvr

case class RawGene(
                  dogGene: String,
                  domRecGene: String
                  )

case class DogContext(age: DogAge)

case class MasterDogGene
  (randomSeed: String,
  // This map WILL contain ALL cases of GeneticProperty
   genes: Map[GeneticProperty, String],
   domRecGenes: Vector[DomRecGene],
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

  def selectEyeType(kind: EyeType): MasterDogGene =
    import DomRecGeneProperty.{None => _, *}
    val domRecGenes = this.domRecGenes.toArray
    val domRecPropStatus = this.domRecPropertyStatus.to(mut.HashMap)
    def clear(idx: Int): Unit =
      val x = domRecGenes(idx)
      domRecGenes(idx) = x.copy(value = x.shared.defaultValue)

    def set(idx: Int, value: TraitType): Unit =
      domRecGenes(idx) = domRecGenes(idx).copy(value = value)

    clear(DomRecGeneStatic.Indices.smallPupils)
    clear(DomRecGeneStatic.Indices.eyelids)
    clear(DomRecGeneStatic.Indices.oblongEyes)
    clear(DomRecGeneStatic.Indices.multiPupils)
    clear(DomRecGeneStatic.Indices.longEyes)
    clear(DomRecGeneStatic.Indices.horizontalEyes)
    clear(DomRecGeneStatic.Indices.triangleEyes)
    clear(DomRecGeneStatic.Indices.missingPupilsEyes)
    clear(DomRecGeneStatic.Indices.decorativeEyes)
    clear(DomRecGeneStatic.Indices.lashesEyes)
    clear(DomRecGeneStatic.Indices.spiralEyes)
    clear(DomRecGeneStatic.Indices.triangleEyes2)
    clear(DomRecGeneStatic.Indices.geometricEyes)

    domRecPropStatus(SmallPupils) = false
    domRecPropStatus(Eyelids) = false
    domRecPropStatus(OblongEyes) = false
    domRecPropStatus(MultiPupils) = false
    domRecPropStatus(LongEyes) = false
    domRecPropStatus(HorizontalEyes) = false
    domRecPropStatus(TriangleEyes) = false
    domRecPropStatus(MissingPupilEyes) = false
    domRecPropStatus(DecorativeEyes) = false
    domRecPropStatus(LashesEyes) = false
    domRecPropStatus(SpiralEyes) = false
    domRecPropStatus(GeometricEyes) = false

    kind match
      // standard is the default case if nothing is true
      case EyeType.Standard => ()
      case EyeType.Lidded =>
        set(DomRecGeneStatic.Indices.eyelids, TraitType.Sub)
        domRecPropStatus(Eyelids) = true
      case EyeType.Oblong =>
        set(DomRecGeneStatic.Indices.oblongEyes, TraitType.Sub)
        domRecPropStatus(OblongEyes) = true
      case EyeType.Concerned =>
        set(DomRecGeneStatic.Indices.smallPupils, TraitType.Sub)
        domRecPropStatus(SmallPupils) = true
      case EyeType.Spider =>
        set(DomRecGeneStatic.Indices.smallPupils, TraitType.Sub)
        set(DomRecGeneStatic.Indices.multiPupils, TraitType.Sub)
        domRecPropStatus(SmallPupils) = true
        domRecPropStatus(MultiPupils) = true
      case EyeType.Hex =>
        set(DomRecGeneStatic.Indices.geometricEyes, TraitType.Sub)
        set(DomRecGeneStatic.Indices.spiralEyes, TraitType.Sub)
        domRecPropStatus(GeometricEyes) = true
        domRecPropStatus(SpiralEyes) = true
      case EyeType.Double =>
        set(DomRecGeneStatic.Indices.oblongEyes, TraitType.Sub)
        set(DomRecGeneStatic.Indices.multiPupils, TraitType.Sub)
        domRecPropStatus(OblongEyes) = true
        domRecPropStatus(MultiPupils) = true
      case EyeType.Keyhole =>
        set(DomRecGeneStatic.Indices.decorativeEyes, TraitType.Sub)
        domRecPropStatus(DecorativeEyes) = true
      case EyeType.Lashes =>
        set(DomRecGeneStatic.Indices.lashesEyes, TraitType.Sub)
        domRecPropStatus(LashesEyes) = true
      case EyeType.Puck =>
        set(DomRecGeneStatic.Indices.horizontalEyes, TraitType.Sub)
        set(DomRecGeneStatic.Indices.oblongEyes, TraitType.Sub)
        domRecPropStatus(HorizontalEyes) = true
        domRecPropStatus(OblongEyes) = true
      case EyeType.Slim =>
        set(DomRecGeneStatic.Indices.horizontalEyes, TraitType.Sub)
        set(DomRecGeneStatic.Indices.longEyes, TraitType.Sub)
        domRecPropStatus(HorizontalEyes) = true
        domRecPropStatus(LongEyes) = true
      case EyeType.Spiral =>
        set(DomRecGeneStatic.Indices.spiralEyes, TraitType.Sub)
        domRecPropStatus(SpiralEyes) = true
      case EyeType.Square =>
        set(DomRecGeneStatic.Indices.geometricEyes, TraitType.Sub)
        domRecPropStatus(GeometricEyes) = true
      case EyeType.Pupilless =>
        set(DomRecGeneStatic.Indices.missingPupilsEyes, TraitType.Sub)
        domRecPropStatus(MissingPupilEyes) = true
      case EyeType.Triangle =>
        set(DomRecGeneStatic.Indices.triangleEyes, TraitType.Sub)
        domRecPropStatus(TriangleEyes) = true
      case EyeType.Mitosis =>
        set(DomRecGeneStatic.Indices.missingPupilsEyes, TraitType.Sub)
        set(DomRecGeneStatic.Indices.multiPupils, TraitType.Sub)
        domRecPropStatus(MissingPupilEyes) = true
        domRecPropStatus(MultiPupils) = true

    copy(domRecGenes = domRecGenes.toVector, domRecPropertyStatus = domRecPropStatus.toMap)

  def selectWingType(kind: WingType): MasterDogGene =
    import DomRecGeneProperty.{None => _, *}
    val domRecGenes = this.domRecGenes.toArray
    val domRecPropStatus = this.domRecPropertyStatus.to(mut.HashMap)

    def set(idx: Int, value: TraitType): Unit =
      domRecGenes(idx) = domRecGenes(idx).copy(value = value)

    if kind == WingType.NoWings then
      set(DomRecGeneStatic.Indices.noWings, TraitType.Dom)
      domRecPropStatus(NoWings) = true
    else
      set(DomRecGeneStatic.Indices.noWings, TraitType.Sub)
      domRecPropStatus(NoWings) = false
      set(DomRecGeneStatic.Indices.wingFeathers, if kind.withFeathers then TraitType.Sub else TraitType.Dom)
      domRecPropStatus(WingFeathers) = kind.withFeathers
      kind match
        case WingType.Angel =>
          set(DomRecGeneStatic.Indices.alignment, TraitType.Dom) // good alignment
          domRecPropStatus(AlignmentGood) = true
          domRecPropStatus(AlignmentNeutral) = false
          domRecPropStatus(AlignmentEvil) = false


        case WingType.Paradise | WingType.Vestigial =>
          set(DomRecGeneStatic.Indices.alignment, TraitType.Het)
          domRecPropStatus(AlignmentGood) = false
          domRecPropStatus(AlignmentNeutral) = true
          domRecPropStatus(AlignmentEvil) = false
        case WingType.Vulture | WingType.Bat =>
          set(DomRecGeneStatic.Indices.alignment, TraitType.Sub)
          domRecPropStatus(AlignmentGood) = false
          domRecPropStatus(AlignmentNeutral) = false
          domRecPropStatus(AlignmentEvil) = true
        case _ => ()

    copy(domRecGenes = domRecGenes.toVector, domRecPropertyStatus = domRecPropStatus.toMap)

  def selectEarType(kind: EarType): MasterDogGene =
    import DomRecGeneProperty.{None => _, *}
    import DomRecGeneStatic.Indices

    val domRecGenes = this.domRecGenes.toArray
    val domRecPropStatus = this.domRecPropertyStatus.to(mut.HashMap)

    def set(idx: Int, value: TraitType): Unit =
      domRecGenes(idx) = domRecGenes(idx).copy(value = value)

    set(Indices.earSharp, TraitType.Dom)
    set(Indices.earConic, TraitType.Dom)
    set(Indices.earFilled, TraitType.Dom)
    set(Indices.earFloppiness, TraitType.Dom)
    set(Indices.earHalved, TraitType.Dom)
    set(Indices.tiltedEars, TraitType.Dom)

    domRecPropStatus(EarSharp) = false
    domRecPropStatus(EarConic) = false
    domRecPropStatus(EarFilled) = false
    domRecPropStatus(EarFloppy) = false
    domRecPropStatus(EarPartialFlop) = false
    domRecPropStatus(EarHalved) = false
    domRecPropStatus(TiltedEars) = false


    kind match
      case EarType.TypeA =>
        set(Indices.earFilled, TraitType.Sub)
        domRecPropStatus(EarFilled) = true
      case EarType.TypeB =>
        set(Indices.earFilled, TraitType.Sub)
        domRecPropStatus(EarFilled) = true
        set(Indices.tiltedEars, TraitType.Het)
        domRecPropStatus(TiltedEars) = true
      case EarType.Blunt =>
        set(Indices.earFilled, TraitType.Sub)
        domRecPropStatus(EarFilled) = true
        // the prop is genuinely in the Het position
        set(Indices.earHalved, TraitType.Het)
        domRecPropStatus(EarHalved) = true
      case EarType.Bent =>
        set(Indices.earFloppiness, TraitType.Het)
        domRecPropStatus(EarFloppy) = true
      case EarType.Bulbous =>
        set(Indices.earSharp, TraitType.Sub)
        domRecPropStatus(EarSharp) = true
        // select EarFloppy
        set(Indices.earFloppiness, TraitType.Het)
        domRecPropStatus(EarFloppy) = true
      case EarType.Horn =>
        set(Indices.earConic, TraitType.Sub)
        domRecPropStatus(EarConic) = true
      case EarType.Cross =>
        set(Indices.earSharp, TraitType.Sub)
        domRecPropStatus(EarSharp) = true
      case EarType.Twisted =>
        set(Indices.earSharp, TraitType.Sub)
        domRecPropStatus(EarSharp) = true

        set(Indices.earConic, TraitType.Sub)
        domRecPropStatus(EarConic) = true

        set(Indices.earHalved, TraitType.Het)
        domRecPropStatus(EarHalved) = true
      // shepherd is the default case
      case EarType.Shepherd => ()
      case EarType.Wavy =>
        // select ear partial flop
        set(Indices.earFloppiness, TraitType.Sub)
        domRecPropStatus(EarPartialFlop) = true

    copy(domRecGenes = domRecGenes.toVector, domRecPropertyStatus = domRecPropStatus.toMap)

  def selectHornType(kind: HornType): MasterDogGene =
    import DomRecGeneProperty.{None => _, *}
    import DomRecGeneStatic.Indices

    val domRecGenes = this.domRecGenes.toArray
    val domRecPropStatus = this.domRecPropertyStatus.to(mut.HashMap)

    def set(idx: Int, value: TraitType): Unit =
      domRecGenes(idx) = domRecGenes(idx).copy(value = value)

    set(Indices.noHorns, TraitType.Sub)
    set(Indices.hornsNub, TraitType.Dom)
    set(Indices.hornsThin, TraitType.Dom)
    set(Indices.hornsThick, TraitType.Dom)
    set(Indices.hornsCurled, TraitType.Dom)

    domRecPropStatus(HornsNone) = false
    domRecPropStatus(HornsNub) = false
    domRecPropStatus(HornsThin) = false
    domRecPropStatus(HornsThick) = false
    domRecPropStatus(HornsCurled) = false

    kind match
      case HornType.NoHorns =>
        set(Indices.noHorns, TraitType.Dom)
        domRecPropStatus(HornsNone) = true
      case HornType.Nub =>
        set(Indices.hornsNub, TraitType.Sub)
        domRecPropStatus(HornsNub) = true
      case HornType.Thick =>
        set(Indices.hornsThick, TraitType.Sub)
        domRecPropStatus(HornsThick) = true
      case HornType.Thin =>
        set(Indices.hornsThin, TraitType.Sub)
        domRecPropStatus(HornsThin) = true
      case HornType.Curled =>
        set(Indices.hornsCurled, TraitType.Sub)
        domRecPropStatus(HornsCurled) = true

    copy(domRecGenes = domRecGenes.toVector, domRecPropertyStatus = domRecPropStatus.toMap)
  def selectHornPlacement(kind: HornPlacement): MasterDogGene =
    import DomRecGeneProperty.{None => _, *}
    import DomRecGeneStatic.Indices

    val domRecGenes = this.domRecGenes.toArray
    val domRecPropStatus = this.domRecPropertyStatus.to(mut.HashMap)

    def set(idx: Int, value: TraitType): Unit =
      domRecGenes(idx) = domRecGenes(idx).copy(value = value)

    set(Indices.centerHorns, TraitType.Dom)
    set(Indices.traditionalHorns, TraitType.Dom)

    domRecPropStatus(HornsCenter) = false
    domRecPropStatus(HornsTraditional) = false

    kind match
      case HornPlacement.Center =>
        set(Indices.centerHorns, TraitType.Sub)
        domRecPropStatus(HornsCenter) = true
      case HornPlacement.Traditional =>
        set(Indices.traditionalHorns, TraitType.Sub)
        domRecPropStatus(HornsTraditional) = true
      case _ => ()

    copy(domRecGenes = domRecGenes.toVector, domRecPropertyStatus = domRecPropStatus.toMap)

  def selectTailType(kind: TailType): MasterDogGene =
    import DomRecGeneProperty.{None => _, *}
    import DomRecGeneStatic.Indices

    val domRecGenes = this.domRecGenes.toArray
    val domRecPropStatus = this.domRecPropertyStatus.to(mut.HashMap)

    def set(idx: Int, value: TraitType): Unit =
      domRecGenes(idx) = domRecGenes(idx).copy(value = value)

    set(Indices.noNubTail, TraitType.Sub)
    set(Indices.tailCurl, TraitType.Dom)
    set(Indices.stiffTail, TraitType.Sub)
    set(Indices.flatTail, TraitType.Dom)
    set(Indices.bulbousTail, TraitType.Dom)
    set(Indices.repeatedTail, TraitType.Dom)
    set(Indices.thinTail, TraitType.Dom)
    set(Indices.tail3D, TraitType.Dom)

    domRecPropStatus(NoTail) = false
    domRecPropStatus(NubTail) = false
    domRecPropStatus(CurledTail) = false
    domRecPropStatus(SlightlyCurledTail) = false
    domRecPropStatus(StiffTail) = false
    domRecPropStatus(FlatTail) = false
    domRecPropStatus(BulbousTail) = false
    domRecPropStatus(RepeatedTail) = false
    domRecPropStatus(ThinTail) = false
    domRecPropStatus(Tail3D) = false

    case class TailFields
      (noTail: Boolean = false
      ,thinTail: Boolean = false
      ,nubTail: Boolean = false
      ,flatTail: Boolean = false
      ,stiffTail: Boolean = false
      ,bulbousTail: Boolean = false
      ,tail3D: Boolean = false
      ,repeatedTail: Boolean = false
      ,curledTail: Boolean = false
      ,slightlyCurledTail: Boolean = false)

    val fields =
      kind match
        case TailType.NoTail =>
          TailFields(noTail = true)
        case TailType.Stiff =>
          TailFields(stiffTail = true)
        case TailType.StiffCurly =>
          TailFields(stiffTail = true, curledTail = true)
        case TailType.Flowy =>
          // the default if tail3D is false
          TailFields()
        case TailType.Nub =>
          TailFields(nubTail = true)
        case TailType.StiffSlightlyCurly =>
          TailFields(stiffTail = true, slightlyCurledTail = true)
        case TailType.Bulbous =>
          TailFields(tail3D = true, bulbousTail = true)
        case TailType.Feral =>
          TailFields(tail3D = true, repeatedTail = true, flatTail = true)
        case TailType.Lifted =>
          TailFields(tail3D = true, nubTail = true, slightlyCurledTail = true)
        case TailType.Paddle =>
          TailFields(tail3D = true, flatTail = true)
        case TailType.Plume =>
          TailFields(tail3D = true)
        case TailType.Whip =>
          TailFields(tail3D = true, thinTail = true, stiffTail = true)
        case TailType.Curl =>
          TailFields(tail3D = true, repeatedTail = true, slightlyCurledTail = true)
        case TailType.DoubleCurl =>
          TailFields(tail3D = true, repeatedTail = true, curledTail = true)
        case TailType.Tri =>
          TailFields(tail3D = true, bulbousTail = true, flatTail = true)

    if fields.noTail then
      set(Indices.noNubTail, TraitType.Dom)
      domRecPropStatus(NoTail) = true

    if fields.thinTail then
      set(Indices.thinTail, TraitType.Sub)
      domRecPropStatus(ThinTail) = true

    if fields.nubTail then
      set(Indices.noNubTail, TraitType.Het)
      domRecPropStatus(NubTail) = true

    if fields.flatTail then
      set(Indices.flatTail, TraitType.Sub)
      domRecPropStatus(FlatTail) = true

    if fields.stiffTail then
      set(Indices.stiffTail, TraitType.Dom)
      domRecPropStatus(StiffTail) = true

    if fields.bulbousTail then
      set(Indices.bulbousTail, TraitType.Sub)
      domRecPropStatus(BulbousTail) = true

    if fields.tail3D then
      set(Indices.tail3D, TraitType.Sub)
      domRecPropStatus(Tail3D) = true

    if fields.repeatedTail then
      set(Indices.repeatedTail, TraitType.Sub)
      domRecPropStatus(RepeatedTail) = true

    if fields.curledTail then
      set(Indices.tailCurl, TraitType.Sub)
      domRecPropStatus(CurledTail) = true

    if fields.slightlyCurledTail then
      set(Indices.tailCurl, TraitType.Het)
      domRecPropStatus(SlightlyCurledTail) = true

    copy(domRecGenes = domRecGenes.toVector, domRecPropertyStatus = domRecPropStatus.toMap)
  def selectNoseType(kind: NoseType): MasterDogGene =
    import DomRecGeneProperty.{None => _, *}
    import DomRecGeneStatic.Indices

    val domRecGenes = this.domRecGenes.toArray
    val domRecPropStatus = this.domRecPropertyStatus.to(mut.HashMap)

    def set(idx: Int, value: TraitType): Unit =
      domRecGenes(idx) = domRecGenes(idx).copy(value = value)

    set(Indices.noseFlat, TraitType.Dom)
    set(Indices.noseRepeated, TraitType.Dom)
    set(Indices.noseExtrusion, TraitType.Dom)
    set(Indices.noseSquishStretch, TraitType.Het)

    domRecPropStatus(NoseFlat) = false
    domRecPropStatus(NoseRepeated) = false
    domRecPropStatus(NoseExtrusion) = false
    domRecPropStatus(NoseSquish) = false
    domRecPropStatus(NoseStretch) = false

    case class NoseFields
      (extrusion: Boolean = false
      ,flat: Boolean = false
      ,squish: Boolean = false
      ,stretch: Boolean = false
      ,repeated: Boolean = false
      )

    val NoseFields(hasExtrusion, hasFlat, hasSquish, hasStretch, hasRepeated) =
      kind match
        case NoseType.Wide =>
          NoseFields(extrusion = true, repeated = true, squish = true)
        case NoseType.Square =>
          NoseFields(repeated = true, squish = true)
        case NoseType.Pug =>
          NoseFields(repeated = true, extrusion = true)
        case NoseType.Mallow =>
          NoseFields(repeated = true, flat = true)
        case NoseType.Triangle =>
          NoseFields(flat = true, stretch = true)
        case NoseType.Greyhound =>
          NoseFields(stretch = true)
        case NoseType.Bulb =>
          NoseFields(extrusion = true)
        case NoseType.HalfMallow =>
          NoseFields(flat = true)
        case NoseType.TypeA =>
          NoseFields()

    if hasExtrusion then
      // yes it IS het
      set(Indices.noseExtrusion, TraitType.Het)
      domRecPropStatus(NoseExtrusion) = true

    if hasFlat then
      set(Indices.noseFlat, TraitType.Sub)
      domRecPropStatus(NoseFlat) = true

    if hasSquish then
      set(Indices.noseSquishStretch, TraitType.Dom)
      domRecPropStatus(NoseSquish) = true

    if hasStretch then
      set(Indices.noseSquishStretch, TraitType.Sub)
      domRecPropStatus(NoseStretch) = true

    if hasRepeated then
      set(Indices.noseRepeated, TraitType.Sub)
      domRecPropStatus(NoseRepeated) = true



    copy(domRecGenes = domRecGenes.toVector, domRecPropertyStatus = domRecPropStatus.toMap)

  def selectMouthType(kind: MouthType): MasterDogGene =
    import DomRecGeneProperty.{None => _, *}
    import DomRecGeneStatic.Indices

    val domRecGenes = this.domRecGenes.toArray
    val domRecPropStatus = this.domRecPropertyStatus.to(mut.HashMap)

    def set(idx: Int, value: TraitType): Unit =
      val x = domRecGenes(idx)
      domRecPropStatus(x.shared.dom) = false
      domRecPropStatus(x.shared.het) = false
      domRecPropStatus(x.shared.sub) = false
      domRecPropStatus(x.shared(value).currentProperty) = true
      domRecGenes(idx) = x.copy(value = value)

    set(Indices.teeth, TraitType.Dom)
    set(Indices.vMouth, TraitType.Dom)
    set(Indices.openMouth, TraitType.Dom)
    // select smile, it does nothing without any other selection
    set(Indices.mouthEmotion, TraitType.Dom)
    set(Indices.mouthCheeks, TraitType.Dom)
    set(Indices.mouthMissingTeeth, TraitType.Dom)
    set(Indices.mouthPointed, TraitType.Dom)
    set(Indices.mouthCutoff, TraitType.Dom)
    set(Indices.mouthWiggle, TraitType.Dom)


    case class MouthFields
      (teeth: Boolean = false
      ,vMouth: Boolean = false
      ,smile: Boolean = false
      ,frown: Boolean = false
      ,cheeks: Boolean = false
      ,cutoff: Boolean = false
      ,wiggle: Boolean = false
      ,openMouth: Boolean = false
      ,pointed: Boolean = false
      ,neutral: Boolean = false
      ,missingTeeth: Boolean = false)

    val fields =
      kind match
        case MouthType.Standard =>
          MouthFields(teeth = true, neutral = true)
        case MouthType.MouthNone =>
          MouthFields()
        case MouthType.Simple =>
          MouthFields(vMouth = true)
        case MouthType.Ah =>
          MouthFields(vMouth = true, openMouth = true)
        case MouthType.Boom =>
          MouthFields(teeth = true, frown = true)
        case MouthType.Cheeky =>
          MouthFields(cheeks = true)
        case MouthType.Diamond =>
          MouthFields(teeth = true, pointed = true)
        case MouthType.Wise =>
          MouthFields(teeth = true, cutoff = true)
        case MouthType.MouthBreather =>
          MouthFields(missingTeeth = true)
        case MouthType.Pointed =>
          MouthFields(frown = true)
        case MouthType.Smug =>
          MouthFields(teeth = true, smile = true)
        case MouthType.Toothy =>
          MouthFields(missingTeeth = true, pointed = true)
        case MouthType.Blank =>
          MouthFields(neutral = true)
        case MouthType.Wobbly =>
          MouthFields(teeth = true, wiggle = true)

    if fields.teeth then
      set(Indices.teeth, TraitType.Sub)
    if fields.vMouth then
      set(Indices.vMouth, TraitType.Sub)
    if fields.openMouth then
      set(Indices.openMouth, TraitType.Sub)
    if fields.smile then
      set(Indices.mouthEmotion, TraitType.Dom)
    if fields.neutral then
      set(Indices.mouthEmotion, TraitType.Het)
    if fields.frown then
      set(Indices.mouthEmotion, TraitType.Sub)
    if fields.cheeks then
      set(Indices.mouthCheeks, TraitType.Sub)
    if fields.missingTeeth then
      set(Indices.mouthMissingTeeth, TraitType.Sub)
    if fields.pointed then
      set(Indices.mouthPointed, TraitType.Sub)
    if fields.cutoff then
      set(Indices.mouthCutoff, TraitType.Sub)
    if fields.wiggle then
      set(Indices.mouthWiggle, TraitType.Sub)

    copy(domRecGenes = domRecGenes.toVector, domRecPropertyStatus = domRecPropStatus.toMap)
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


  def hornType: HornType = {
    val hornsCenter = this.domRecPropertyStatus(DomRecGeneProperty.HornsCenter)
    val hornsTraditional = this.domRecPropertyStatus(DomRecGeneProperty.HornsTraditional)
    val noHorns = this.domRecPropertyStatus(DomRecGeneProperty.HornsNone)
    val hornsCurled = this.domRecPropertyStatus(DomRecGeneProperty.HornsCurled)
    val hornsNub = this.domRecPropertyStatus(DomRecGeneProperty.HornsNub)
    val hornsThick = this.domRecPropertyStatus(DomRecGeneProperty.HornsThick)
    val hornsThin = this.domRecPropertyStatus(DomRecGeneProperty.HornsThin)
    // surpress the disabling of horns so it can be displayed in editor
    if (noHorns /* || (!hornsCenter && !hornsTraditional) */ )
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

  val noseType: NoseType = {
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

  def hornPlacement: HornPlacement = {
    val centerHorn = this.domRecPropertyStatus(DomRecGeneProperty.HornsCenter)
    val traditionalHorns = this.domRecPropertyStatus(DomRecGeneProperty.HornsTraditional)
    if (centerHorn)
      HornPlacement.Center
    else if (traditionalHorns)
      HornPlacement.Traditional
    else
      HornPlacement.None
  }

  def tailType: TailType = {
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

  def eyeType: EyeType = {
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

  def mouthType: MouthType = {
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
    }.toVector
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
