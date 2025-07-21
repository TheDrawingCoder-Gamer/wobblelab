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




case class CalculatedValue(value: Float, percentage: Float)

case class CalculatedGenes(
                          bodyMat: CalculatedMaterial,
                          legColor: CalculatedMaterial,
                          noseEarColor: CalculatedMaterial,
                          floatItems: Map[Gene, CalculatedValue],
                          headNumber: Int,
                          tailNumber: Int,
                          wingNumber: Int,
                          frontLegPairs: Int,
                          backLegPairs: Int,
                          // TODO: voice
                          // TODO: ear curl, ear mod a
                          earType: EarType,
                          eyeType: EyeType,
                          hornType: HornType,
                          hornPlacement: HornPlacement,
                          mouthType: MouthType,
                          // TODO: nose mod a
                          noseType: NoseType,
                          tailType: TailType,
                          wingType: WingType
                          )
