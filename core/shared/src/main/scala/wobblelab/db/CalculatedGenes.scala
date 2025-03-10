package net.bulbyvr
package wobblelab
package db


case class CalculatedMaterial(
                             base: util.ColorF,
                             emission: util.ColorF,
                             metallic: Float,
                             glossiness: Float
                             )

object CalculatedMaterial {
  val DEFAULT: CalculatedMaterial =
    CalculatedMaterial(util.ColorF.WHITE, util.ColorF.WHITE, 1.0f, 1.0f)
}

case class CalculatedGenes(
                          bodyMat: CalculatedMaterial,
                          legColor: CalculatedMaterial,
                          noseEarColor: CalculatedMaterial,
                          floatItems: Map[String, Float],
                          headNumber: Int,
                          tailNumber: Int,
                          wingNumber: Int,
                          frontLegPairs: Int,
                          backLegPairs: Int,
                          // TODO: voice
                          // TODO: ear curl, ear mod a
                          earType: EarType,
                          /* TODO: Eye type and mouth type
                           ARE done via code, but they aren't
                           named _in the code_. have to find localization
                           strings (or experiment with it )
                           */
                          // eyeType: EyeType,
                          hornType: HornType,
                          hornPlacement: HornPlacement,
                          // mouthType: MouthType,
                          // TODO: nose mod a
                          noseType: NoseType,
                          tailType: TailType,
                          wingType: WingType
                          )
