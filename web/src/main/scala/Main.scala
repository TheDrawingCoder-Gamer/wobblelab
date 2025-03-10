import scala.annotation.experimental
import calico.*
import calico.html.{Children, Modifier}
import fs2.dom.*
import calico.html.io.{*, given}
import cats.effect.*
import cats.syntax.all.*
import cats.effect.syntax.all.*
import fs2.*
import fs2.concurrent.*
import macros.imapCopied
import net.bulbyvr.wobblelab.db.MasterDogGene
import org.scalajs.dom

import scala.compiletime.{codeOf, constValue, erasedValue, error, summonInline}
import scala.deriving.Mirror


@experimental
object Main extends IOWebApp {
  import net.bulbyvr.wobblelab.*
  import RawDog.experimental.*

  inline def summonSingletonCases[T <: Tuple, A](inline typeName: Any): List[A] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t) =>
        inline summonInline[Mirror.Of[h]] match
          case m: Mirror.Singleton => m.fromProduct(EmptyTuple).asInstanceOf[A] :: summonSingletonCases[t, A](typeName)
          case m: Mirror =>
            error("Enum " + codeOf(typeName) + " contains non singleton case " + codeOf(constValue[m.MirroredLabel]))

  def resultPane(dog: SignallingRef[IO, GameDog]): Resource[IO, HtmlElement[IO]] = {
    val calculatedSignal = dog.map(_.masterGene.calculateGenes())
    div(
      p("Horn type: ", calculatedSignal.map(_.hornType.toString)),
      p("Horn placement: ", calculatedSignal.map(_.hornPlacement.display)),
      p("Ear type: ", calculatedSignal.map(_.earType.toString)),
      p("Nose type: ", calculatedSignal.map(_.noseType.toString)),
      p("Tail type: ", calculatedSignal.map(_.tailType.toString)),
      p("Wing type: ", calculatedSignal.map(_.wingType.toString)),
      p("Front leg pairs: ", calculatedSignal.map(_.frontLegPairs.toString)),
      p("Back leg pairs: ", calculatedSignal.map(_.backLegPairs.toString)),
      p("Wing number: ", calculatedSignal.map(_.wingNumber.toString)),
      p("Tail number: ", calculatedSignal.map(_.tailNumber.toString)),
      p("Head number: ", calculatedSignal.map(_.headNumber.toString))
    )
  }
  def generalPane(dog: SignallingRef[IO, GameDog]): Resource[IO,HtmlElement[IO]] = {
    val ageSignal = dog.imapCopied[DogAge]("age")
    val ageProgress = dog.imapCopied[Float]("ageProgress")
    div(
      strCompTextbox("Name:", dog.imapCopied("dogName")),
      p(
        "Age:",
        select.withSelf { self =>
          (
            DogAge.values.filter(_ != DogAge.Empty).map(it => option(value := it.ordinal.toString, it.toString)).toList,
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
  }
  def dualGeneGroup(name: String, dualGene: SignallingRef[IO, DualGene]): Resource[IO, HtmlElement[IO]] = {
    div(
      strCompTextbox(name + " Plus:", dualGene.imapCopied[String]("plus")),
      strCompTextbox(name + " Minus:", dualGene.imapCopied[String]("minus")),
    )
  }
  def colorGeneGroup(name: String, colorGene: SignallingRef[IO, ColorGene]): Resource[IO, HtmlElement[IO]] = {
    div(
      strCompTextbox(s"$name Emission Color (Red) Plus", colorGene.imapCopied[String]("emissionRedPlus")),
      strCompTextbox(s"$name Emission Color (Red) Minus", colorGene.imapCopied[String]("emissionRedMinus")),
      strCompTextbox(s"$name Emission Color (Green) Plus", colorGene.imapCopied[String]("emissionGreenPlus")),
      strCompTextbox(s"$name Emission Color (Green) Minus", colorGene.imapCopied[String]("emissionGreenMinus")),
      strCompTextbox(s"$name Emission Color (Blue) Plus", colorGene.imapCopied[String]("emissionBluePlus")),
      strCompTextbox(s"$name Emission Color (Blue) Minus", colorGene.imapCopied[String]("emissionBlueMinus")),
      strCompTextbox(s"$name Base Color (Red) Plus", colorGene.imapCopied[String]("baseRedPlus")),
      strCompTextbox(s"$name Base Color (Red) Minus", colorGene.imapCopied[String]("baseRedMinus")),
      strCompTextbox(s"$name Base Color (Green) Plus", colorGene.imapCopied[String]("baseGreenPlus")),
      strCompTextbox(s"$name Base Color (Green) Minus", colorGene.imapCopied[String]("baseGreenMinus")),
      strCompTextbox(s"$name Base Color (Blue) Plus", colorGene.imapCopied[String]("baseBluePlus")),
      strCompTextbox(s"$name Base Color (Blue) Minus", colorGene.imapCopied[String]("baseBlueMinus")),
    )
  }
  def shinyGeneGroup(name: String, shinyGene: SignallingRef[IO, ShinyGene]): Resource[IO, HtmlElement[IO]] = {
    div(
      strCompTextbox(name + " Metallic Plus", shinyGene.imapCopied("metallicPlus")),
      strCompTextbox(name + " Metallic Minus", shinyGene.imapCopied[String]("metallicMinus")),
      strCompTextbox(name + " Gloss Plus", shinyGene.imapCopied[String]("glossPlus")),
      strCompTextbox(name + " Gloss Minus", shinyGene.imapCopied[String]("glossMinus"))
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
                         (it => it.getGeneString(key).getOrElse(""),
                           src => it => src.updatedGeneString(key, it)))
        }.toList
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
                _.evalMap(_ => self.value.get).foreach(v => masterGene.set(gene.copy(domRecGenes = gene.domRecGenes.updated(idx, it.copy(value = db.TraitType.valueOf(v))))))
              }
            )}
            )
          }
        }
      )
    )
  }
  inline def personalityBinder[T <: scala.reflect.Enum](personalitySignal: SignallingRef[IO, T], name: String)(using mirror: deriving.Mirror.SumOf[T]): Resource[IO, HtmlElement[IO]] = {
    val cases = summonSingletonCases[mirror.MirroredElemTypes, T](constValue[mirror.MirroredLabel])
    val caseOf = cases.toVector
    div(
      cls := "personalityLine",
      name,
      select.withSelf {self =>
        (
          cases.map(it => option(value := it.ordinal.toString, it.toString)),
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
      personalityBinder[SocialPersonality](socialPersonality, "Social: "),
      personalityBinder[EnergyPersonality](energyPersonality, "Energy: "),
      personalityBinder[FoodPersonality](foodPersonality, "Food: "),
      personalityBinder[MischiefPersonality](mischiefPersonality, "Mischief: "),
      personalityBinder[NicenessPersonality](nicenessPersonality, "Niceness: "),
      personalityBinder[PettablePersonality](pettablePersonality, "Pettable: "),
      personalityBinder[LoudnessPersonality](loudnessPersonality, "Loudness: ")
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
      blawg <- SignallingRef[IO].of(Dog.randy.asRawDog.toGameDog).toResource
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
              tabs(selectedTab)("General", "Standard Genes", "Dom Rec Genes", "Personality", "results")
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
