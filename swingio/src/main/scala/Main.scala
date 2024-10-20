import scala.annotation.experimental
import net.bulbyvr.swing.io.all.{*, given}
import net.bulbyvr.swing.io.wrapper.*
import net.bulbyvr.swing.io.wrapper.event.*
import cats.effect.*
import cats.syntax.all.*
import cats.effect.syntax.all.*
import fs2.concurrent.*
import macros.imapCopied
import net.bulbyvr.swing.io.IOSwingApp

@experimental
object Main extends IOSwingApp {
  import net.bulbyvr.wobblelab.*

  def generalPane(dog: SignallingRef[IO, Dog]): Resource[IO,Component[IO]] = {
    val ageSignal = dog.imapCopied[DogAge]("age")
    val ageProgress = dog.imapCopied[Float]("ageProgress")
    box(
      strCompTextbox("Name:", dog.imapCopied("dogName")),
      flow(
        "Age:",
        comboBox[DogAge].withSelf { self =>
          (
            items := DogAge.values.filter(_ != DogAge.Empty).toSeq,
            onSelectionChange --> {
              _.evalMap(_ => self.item.get).foreach(ageSignal.set)
            },
            item <-- ageSignal
          )
        }
      ),
      flow(
        "Age Progress:",
        textField.withSelf { self =>
          (
            columns := 20,
            onValueChange --> {
              _.evalMap(_ => self.text.get).foreach(_.toFloatOption.map(it => ageProgress.set(it)).getOrElse(IO.pure(())))
            },
            text <-- ageProgress.map(_.toString)
          )
        }
      ),
      strCompTextbox("Random Seed:", SignallingRef.lens[IO, Dog, String](dog)(_.geneProp0.randomSeed, src => it => src.copy(geneProp0 = src.geneProp0.copy(randomSeed = it))))
    )
  }
  def dualGeneGroup(name: String, dualGene: SignallingRef[IO, DualGene]): Resource[IO, Component[IO]] = {
    box(
      strCompTextbox(name + " Plus:", dualGene.imapCopied[String]("plus")),
      strCompTextbox(name + " Minus:", dualGene.imapCopied[String]("minus")),
    )
  }
  def colorGeneGroup(name: String, colorGene: SignallingRef[IO, ColorGene]): Resource[IO, Component[IO]] = {
    box(
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
  def shinyGeneGroup(name: String, shinyGene: SignallingRef[IO, ShinyGene]): Resource[IO, Component[IO]] = {
    box(
      strCompTextbox(name + " Metallic Plus", shinyGene.imapCopied("metallicPlus")),
      strCompTextbox(name + " Metallic Minus", shinyGene.imapCopied[String]("metallicMinus")),
      strCompTextbox(name + " Gloss Plus", shinyGene.imapCopied[String]("glossPlus")),
      strCompTextbox(name + " Gloss Minus", shinyGene.imapCopied[String]("glossMinus"))
    )
  }

  def strCompTextbox(name: String, str: SignallingRef[IO, String]): Resource[IO, Component[IO]] = {
    flow(
      label(text := name),
      textField.withSelf { self =>
        (
          columns := 20,
          onValueChange --> {
            _.evalMap(_ => self.text.get).foreach(str.set)
          },
          text <-- str
        )
      }
    )
  }

  def standardGenePane(dog: SignallingRef[IO, Dog]): Resource[IO, Component[IO]] = {
    val gene0 = dog.imapCopied[DogGene0]("geneProp0")
    val gene1 = dog.imapCopied[DogGene1]("geneProp1")
    scrollPane(
    box(
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
  def view: Resource[IO, Component[IO]] = {
    for {
      blawg <- SignallingRef[IO].of(Dog.randy).toResource
      res <- {
        box(
          "dog code",
          textArea(
            columns := 50,
            rows := 5,
            text <-- blawg.map(it => DogRegistry.exportDog(it.asRawDog))
          ),
          notebook(
            "General" -> generalPane(blawg),
            "Standard Genes" -> standardGenePane(blawg)
          )
        )
      }
    } yield res
  }

  override def render: Resource[IO, (MainFrame[IO], Deferred[IO, Unit])] = {
    window(
      title := "wobblelab",
      view
    )
  }

}
