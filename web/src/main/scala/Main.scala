import scala.annotation.experimental
import calico.*
import calico.html.Modifier
import fs2.dom.*
import calico.html.io.{*, given}
import cats.effect.*
import cats.syntax.all.*
import cats.effect.syntax.all.*
import fs2.*
import fs2.concurrent.*
import macros.imapCopied
import org.scalajs.dom

@experimental
object Main extends IOWebApp {
  import net.bulbyvr.wobblelab.*

  def generalPane(dog: SignallingRef[IO, Dog]): Resource[IO,HtmlElement[IO]] = {
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
      strCompTextbox("Random Seed:", SignallingRef.lens[IO, Dog, String](dog)(_.geneProp0.randomSeed, src => it => src.copy(geneProp0 = src.geneProp0.copy(randomSeed = it))))
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

  def standardGenePane(dog: SignallingRef[IO, Dog]): Resource[IO, HtmlElement[IO]] = {
    val gene0 = dog.imapCopied[DogGene0]("geneProp0")
    val gene1 = dog.imapCopied[DogGene1]("geneProp1")
    div(
      cls := "scrollView",
      div(
        (
          shinyGeneGroup("Body", gene0.imapCopied[ShinyGene]("bodyShiny")),
          colorGeneGroup("Body", gene0.imapCopied[ColorGene]("bodyColor")),
          shinyGeneGroup("Leg", gene0.imapCopied("legShiny")),
          colorGeneGroup("Leg", gene0.imapCopied("legColor")),
          shinyGeneGroup("Nose/Ear", gene0.imapCopied("noseShiny")),
          colorGeneGroup("Nose/Ear", gene0.imapCopied("noseColor")),
          dualGeneGroup("Nose Size", gene0.imapCopied("noseModA")),
          dualGeneGroup("Horn Size", gene0.imapCopied("hornSize")),
          dualGeneGroup("Ear Length", gene0.imapCopied("earModA")),
          dualGeneGroup("Ear Curl", gene0.imapCopied("earCurl")),
          dualGeneGroup("Snout Rotation", gene0.imapCopied("snoutModA")),
          dualGeneGroup("Snout Length", gene0.imapCopied("snoutModB")),
          dualGeneGroup("Snout Size", gene0.imapCopied("snoutModC")),
          dualGeneGroup("Head Size", gene0.imapCopied("headSize")),
          dualGeneGroup("Wing Size", gene0.imapCopied("wingSize"))
        ),
        (
          dualGeneGroup("Stance Width Front", gene0.imapCopied("stanceWidthFront")),
          dualGeneGroup("Stance Width Back", gene0.imapCopied("stanceWidthBack")),
          colorGeneGroup("Pattern", gene0.imapCopied("patternColor")),
          strCompTextbox("Pattern Intensity", gene0.imapCopied("patternAlpha")),
          shinyGeneGroup("Pattern", gene0.imapCopied("patternShiny")),
          strCompTextbox("Pattern Frequency", gene0.imapCopied("patternNum")),
          strCompTextbox("Pattern Horizontal Flip", gene0.imapCopied("patternFlipX")),
          strCompTextbox("Pattern Frequency", gene0.imapCopied("patternFlipY")),
          strCompTextbox("Pattern Variations", gene0.imapCopied("patternInfo")),
          strCompTextbox("Head Number", gene1.imapCopied("headNumber")),
          dualGeneGroup("Body Scale X", gene1.imapCopied("bodyScaleX")),
          dualGeneGroup("Body Scale Z", gene1.imapCopied("bodyScaleZ")),
          dualGeneGroup("Body Scale Y", gene1.imapCopied("bodyScaleY")),
          dualGeneGroup("Body Girth", gene1.imapCopied("bodyScaleYZ")),
          dualGeneGroup("Body Size", gene1.imapCopied("bodyScaleGlobal")),
          dualGeneGroup("Tail Size", gene1.imapCopied("tailScale")),
        ),
        strCompTextbox("Tail Number", gene1.imapCopied("tailNum")),
        strCompTextbox("Wing Number", gene1.imapCopied("wingNum")),
        dualGeneGroup("Front Leg Girth", gene1.imapCopied("legScaleXZFront")),
        dualGeneGroup("Back Leg Girth", gene1.imapCopied("legScaleXZBack")),
        dualGeneGroup("Front Top Leg Length", gene1.imapCopied("legScaleYFrontTop")),
        dualGeneGroup("Front Bottom Leg Length", gene1.imapCopied("legScaleYFrontBot")),
        dualGeneGroup("Back Top Leg Length", gene1.imapCopied("legScaleYBackTop")),
        dualGeneGroup("Back Bottom Leg Length", gene1.imapCopied("legScaleYBackBot")),
        strCompTextbox("Front Leg Number", gene1.imapCopied("legPairsFront")),
        strCompTextbox("Back Leg Number", gene1.imapCopied("legPairsBack"))
      )
    )
  }


  def render: Resource[IO, HtmlElement[IO]] = {
    for {
      blawg <- SignallingRef[IO].of(Dog.randy).toResource
      selectedTab <- SignallingRef[IO].of(0).toResource
      freakyPanes =
        Vector(
          generalPane(blawg),
          standardGenePane(blawg)
        )
      res <- {
        div(
          "dog code",
          textArea.withSelf { self =>
            (
              rows := 5,
              value <-- blawg.map(it => DogRegistry.exportDog(it.asRawDog)),
              onChange --> {
                _.evalMap(_ => self.value.get).map(DogRegistry.importDog)
                 .unNone
                 .map(_.toDog)
                 .unNone
                 .foreach(blawg.set)
              }
            )
          },
          div(
            div(
              button(
                "General",
                onClick --> {
                  _.foreach(_ => selectedTab.set(0))
                }
              ),
              button(
                "Standard Genes",
                onClick --> {
                  _.foreach(_ => selectedTab.set(1))
                }
              )
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
