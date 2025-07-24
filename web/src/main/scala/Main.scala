import scala.annotation.experimental
import calico.*
import calico.html.{Children, Modifier}
import fs2.dom.*
import calico.html.io.{*, given}
import cats.arrow.FunctionK
import cats.data.*
import cats.effect.*
import cats.syntax.all.*
import cats.effect.syntax.all.*
import fs2.*
import fs2.concurrent.*
import macros.imapCopied
import net.bulbyvr.wobblelab.db.{CalculatedMaterial, Gene, MasterDogGene, TraitType}
import org.scalajs.dom
import dom.console
import net.bulbyvr.wobblelab.util.ColorF

import java.text.DecimalFormat
import scala.compiletime.{codeOf, constValue, erasedValue, error, summonInline}
import scala.deriving.Mirror


object Main extends IOWebApp {
  import net.bulbyvr.wobblelab.{*, given}

  inline def summonSingletonCases[T <: Tuple, A](inline typeName: Any): List[A] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t) =>
        inline summonInline[Mirror.Of[h]] match
          case m: Mirror.Singleton => m.fromProduct(EmptyTuple).asInstanceOf[A] :: summonSingletonCases[t, A](typeName)
          case m: Mirror =>
            error("Enum " + codeOf(typeName) + " contains non singleton case " + codeOf(constValue[m.MirroredLabel]))

  def matColor(part: DogMaterialPart, geneDawg: SignallingRef[IO, MasterDogGene],
               mat: Signal[IO, CalculatedMaterial],
               fieldAccess: CalculatedMaterial => ColorF,
               updateAccess: MasterDogGene => ColorF => MasterDogGene): Resource[IO, HtmlElement[IO]] =
    input.withSelf { self =>
      (
        tpe := "color",
        value <-- mat.map(it => fieldAccess(it).showOpaque),
        onChange --> {
          _.evalMap(_ => self.value.get).foreach(it => geneDawg.modify(gene => {
            (updateAccess(gene)(util.Color.parseHex(it).toFloatColor), ())
          }))
        }
      )
    }

  val percentFormat = new DecimalFormat("0.##%")

  def formatPercent(f: Float): String =
    if f.isNaN || f.isInfinite then
      "???%"
    else
      percentFormat.format(f)
  
  def matSection(dog: SignallingRef[IO, GameDog], mat: Signal[IO, CalculatedMaterial], name: String, part: DogMaterialPart): Resource[IO, HtmlElement[IO]] =
    val geneDawg = SignallingRef.lens[IO, GameDog, MasterDogGene](dog)(_.masterGene, src => gene => {
      val raw = gene.getRawString
      src.asRawDog.copy(dogGene = raw.dogGene, domRecGene = raw.domRecGene).toGameDog
    })
    div(
      p(name + " color: ", mat.map(_.base.showOpaque), "  ", matColor(part, geneDawg, mat, _.base, src => it => src.updatePartBase(part, it))),
      p(name + " emission color: ", mat.map(_.emission.showOpaque), "  ", matColor(part, geneDawg, mat, _.emission, src => it => src.updatePartEmission(part, it))),
      p(name + " metallic: ", mat.map(it => formatPercent(it.metallic))),
      p(name + " glossiness: ", mat.map(it => formatPercent(it.glossiness)))
    )

  def geneFloatResult(dog: SignallingRef[IO, GameDog], calculatedSignal: Signal[IO, db.CalculatedGenes], gene: Gene): Resource[IO, HtmlElement[IO]] = {
    SignallingRef.of[IO, Option[NonEmptyChain[String]]](None).toResource.flatMap: error =>
      p(
        gene.displayName + ": ",
        calculatedSignal.map(it => formatPercent(it.floatItems(gene).percentage)),
        input.withSelf { self =>
          (
            tpe := "text",
            value <-- calculatedSignal.map(it => (it.floatItems(gene).percentage * 100).toString),
            onChange --> {
              _.evalMap(_ => self.value.get).map(_.toFloatOption)
               .unNone
               .foreach(it => dog.flatModify { src =>
                 src.updatedPercent(gene, it / 100f) match
                   case Validated.Invalid(inv) =>
                     (src, error.set(Some(inv)))
                   case Validated.Valid(v) =>
                     (v, error.set(None))
               })
            }
          )
        },
        p(
        error.map {
          case Some(errs) => errs.toList.mkString(", ")
          case None => ""
        },
          cls := "error"
        )
      )
  }


  def resultPane(dog: SignallingRef[IO, GameDog]): Resource[IO, HtmlElement[IO]] = {
    val calculatedSignal = dog.map(_.calculatedGenes)
    val earType = SignallingRef.lens[IO, GameDog, EarType](dog)(_.masterGene.earType, src => it => src.copy(masterGene = src.masterGene.selectEarType(it)))
    val hornType = SignallingRef.lens[IO, GameDog, HornType](dog)(_.masterGene.hornType, src => it => src.copy(masterGene = src.masterGene.selectHornType(it)))
    val hornPlacement = SignallingRef.lens[IO, GameDog, HornPlacement](dog)(_.masterGene.hornPlacement, src => it => src.copy(masterGene = src.masterGene.selectHornPlacement(it)))
    val noseType = SignallingRef.lens[IO, GameDog, NoseType](dog)(_.masterGene.noseType, src => it => src.copy(masterGene = src.masterGene.selectNoseType(it)))
    val tailType = SignallingRef.lens[IO, GameDog, TailType](dog)(_.masterGene.tailType, src => it => src.copy(masterGene = src.masterGene.selectTailType(it)))
    val wingType = SignallingRef.lens[IO, GameDog, WingType](dog)(_.masterGene.wingType, src => it => src.copy(masterGene = src.masterGene.selectWingType(it)))
    val eyeType = SignallingRef.lens[IO, GameDog, EyeType](dog)(_.masterGene.eyeType, src => it => src.copy(masterGene = src.masterGene.selectEyeType(it)))
    val mouthType = SignallingRef.lens[IO, GameDog, MouthType](dog)(_.masterGene.mouthType, src => it => src.copy(masterGene = src.masterGene.selectMouthType(it)))

    div(
      cls := "scrollView",
      div(
        personalityBinder[HornType](hornType, "Horn type: "),
        personalityBinder[HornPlacement](hornPlacement, "Horn placement: "),
        personalityBinder[EarType](earType, "Ear type: "),
        personalityBinder[NoseType](noseType, "Nose type: "),
        personalityBinder[TailType](tailType, "Tail type: "),
        personalityBinder[WingType](wingType, "Wing type: "),
        personalityBinder[EyeType](eyeType, "Eye type: "),
        personalityBinder[MouthType](mouthType, "Mouth type: "),
        div(
          children <-- calculatedSignal.map(_.integralItems.iterator.toList.map { (gene, value) =>
            p(gene.displayName + ": ", value.toString)
          })
        ),
        matSection(dog, calculatedSignal.map(_.bodyMat), "Body", DogMaterialPart.Body),
        matSection(dog, calculatedSignal.map(_.legColor), "Legs", DogMaterialPart.Legs),
        matSection(dog, calculatedSignal.map(_.noseEarColor), "Nose/Ear", DogMaterialPart.EarsNose),
        div(
          db.Gene.floatValues.map(gene => geneFloatResult(dog, calculatedSignal, gene))
        )

      )
    )
  }
  def generalPane(dog: SignallingRef[IO, GameDog]): Resource[IO,HtmlElement[IO]] = {
    val ageSignal = dog.imapCopied[DogAge]("age")
    val ageProgress = dog.imapCopied[Float]("ageProgress")
    div(
      cls := "scrollView",
      div(
        strCompTextbox("Name:", dog.imapCopied("dogName")),
        p(
          "Age:",
          select.withSelf { self =>
            (
              DogAge.values.filter(_ != DogAge.Empty).map(it => option(value := it.ordinal.toString, util.PrettyPrint[DogAge].prettyPrint(it))).toList,
              onChange --> {
                _.evalMap(_ => self.value.get).map(DogAge.parseString).foreach(ageSignal.set)
              },
              value <-- ageSignal.map(_.ordinal.toString)
            )
          }
        ),
        p(
          "Age Progress:",
          input.withSelf { self =>
            (
              tpe := "text",
              onChange --> {
                _.evalMap(_ => self.value.get).foreach(_.toFloatOption.map(it => ageProgress.set(it)).getOrElse(IO.pure(())))
              },
              value <-- ageProgress.map(_.toString)
            )
          }
        ),
        strCompTextbox("Random Seed:", SignallingRef.lens[IO, GameDog, String](dog)(_.masterGene.randomSeed, src => it => src.copy(masterGene = src.masterGene.copy(randomSeed = it))))
      )
    )
  }
  def standardGeneTextbox(len: Int, name: String, str: SignallingRef[IO, String]): Resource[IO, HtmlElement[IO]] = {
    p(
      name,
      input.withSelf { self =>
        (
          onChange --> {
            _.evalMap(_ => self.value.get).foreach(it => if it.length == len then str.set(it) else IO.unit)
          },
          value <-- str
        )
      }
    )
  }

  def strCompTextbox(name: String, str: SignallingRef[IO, String]): Resource[IO, HtmlElement[IO]] = {
    p(
      name,
      input.withSelf { self =>
        (
          onChange --> {
            _.evalMap(_ => self.value.get).foreach(str.set)
          },
          value <-- str
        )
      }
    )
  }

  def standardGenePane(dog: SignallingRef[IO, GameDog]): Resource[IO, HtmlElement[IO]] = {
    val masterGene = dog.imapCopied[db.MasterDogGene]("masterGene")
    div(
      cls := "scrollView",
      div(
        db.GeneticProperty.values.map { key =>
          strCompTextbox(key.displayName,
            SignallingRef.lens[IO, MasterDogGene, String](masterGene)
                         // ??????????
                         (it => it.genes(key),
                           src => it => src.updatedGeneString(key, it).getOrElse(src)))
        }
      )
    )
  }

  def domRecPane(dog: SignallingRef[IO, GameDog]): Resource[IO, HtmlElement[IO]] = {
    val masterGene = dog.imapCopied[db.MasterDogGene]("masterGene")
    div(
      cls := "scrollView",
      div(
        children <-- masterGene.map { gene =>
          gene.domRecGenes.zipWithIndex.map { (it, idx) =>
            div(
              p(s"Gene ${idx + 1}"),
            select.withSelf { self => (
              option(value := "Dom", "Dom " + it.shared.dom.displayName),
              option(value := "Het", "Het " + it.shared.het.displayName),
              option(value := "Sub", "Sub " + it.shared.sub.displayName),
              value := it.value.toString,
              onChange --> {
                _.evalMap(_ => self.value.get).foreach(v => masterGene.set(gene.updateDomRec(idx, TraitType.valueOf(v))))
              }
            )}
            )
          }.toList
        }
      )
    )
  }
  inline def personalityBinder[T <: scala.reflect.Enum](personalitySignal: SignallingRef[IO, T], name: String)(using mirror: deriving.Mirror.SumOf[T], pretty: util.PrettyPrint[T]): Resource[IO, HtmlElement[IO]] = {
    val cases = summonSingletonCases[mirror.MirroredElemTypes, T](constValue[mirror.MirroredLabel])
    val caseOf = cases.toVector
    div(
      cls := "personalityLine",
      name,
      select.withSelf {self =>
        (
          cases.map(it => option(value := it.ordinal.toString, pretty.prettyPrint(it))),
          value <-- personalitySignal.map(_.ordinal.toString),
          onChange --> {
            _.evalMap(_ => self.value.get).map(_.toIntOption).unNone.map(caseOf).foreach(personalitySignal.set)
          }
        )
      }
    )
  }
  def personalityPane(dog: SignallingRef[IO, GameDog]): Resource[IO, HtmlElement[IO]] = {
    val personality = dog.imapCopied[DogPersonality]("personality")
    val socialPersonality  = personality.imapCopied[SocialPersonality]("social")
    val energyPersonality = personality.imapCopied[EnergyPersonality]("energy")
    val foodPersonality = personality.imapCopied[FoodPersonality]("food")
    val mischiefPersonality = personality.imapCopied[MischiefPersonality]("mischief")
    val nicenessPersonality = personality.imapCopied[NicenessPersonality]("niceness")
    val pettablePersonality = personality.imapCopied[PettablePersonality]("pettable")
    val loudnessPersonality = personality.imapCopied[LoudnessPersonality]("loudness")
    div(
      cls := "scrollView",
    div(
      personalityBinder[SocialPersonality](socialPersonality, "Social: "),
      personalityBinder[EnergyPersonality](energyPersonality, "Energy: "),
      personalityBinder[FoodPersonality](foodPersonality, "Food: "),
      personalityBinder[MischiefPersonality](mischiefPersonality, "Mischief: "),
      personalityBinder[NicenessPersonality](nicenessPersonality, "Niceness: "),
      personalityBinder[PettablePersonality](pettablePersonality, "Pettable: "),
      personalityBinder[LoudnessPersonality](loudnessPersonality, "Loudness: ")
    )
    )
  }
  def tab(selectedTab: SignallingRef[IO, Int], name: String, id: Int): Resource[IO, HtmlElement[IO]] = {
    button(
      cls <-- selectedTab.map(it => if (it == id) List("tab", "selected") else List("tab")),
      name,
      onClick --> {
        _.foreach(_ => selectedTab.set(id))
      }
    )
  }
  def tabs(selectedTab: SignallingRef[IO, Int])(names: String*): List[Resource[IO, HtmlElement[IO]]] = {
    names.zipWithIndex.toList.map { (name, idx) =>
      tab(selectedTab, name, idx)
    }
  }
  def render: Resource[IO, HtmlElement[IO]] = {
    for {
      blawg <- SignallingRef[IO].of(Dog.randy.asRawDog.get.toGameDog).toResource
      selectedTab <- SignallingRef[IO].of(0).toResource
      freakyPanes =
        Vector(
          generalPane(blawg),
          standardGenePane(blawg),
          domRecPane(blawg),
          personalityPane(blawg),
          resultPane(blawg)
        )
      res <- {
        div(
          cls := "rootContainer flexContainer",
          "dog code",
          textArea.withSelf { self =>
            (
              rows := 5,
              value <-- blawg.map(it => DogRegistry.exportDog(it.asRawDog)),
              onChange --> {
                _.evalMap(_ => self.value.get).map(DogRegistry.importDog)
                 .unNone
                 .map(_.toGameDog)
                 .foreach(blawg.set)
              }
            )
          },
          div(
            cls := "notebook flexContainer nonScrollable",
            div(
              cls := "tabContainer",
              tabs(selectedTab)("General", "Standard Genes", "Dom Rec Genes", "Personality", "Results")
            ),
            // ?
            selectedTab.map {
               freakyPanes.apply
            }

          )
        )
      }
    } yield res
  }


}
