package net.bulbyvr
package wobblelab.db

import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.yaml.scalayaml.Parser

case class GenesDb(
                  dogGenes: List[DogGeneTemplate],
                  domRecGenes: List[DomRecGeneStatic]
                  )

given genesDbCodec: Codec[GenesDb] = deriveCodec[GenesDb]

object GenesDb {
  lazy val genesDb: GenesDb = Parser.parse(wobblelab.GeneV12_yaml).flatMap(_.as[GenesDb]) match {
    case Right(value) => value
    case Left(err) => {
      throw new RuntimeException(err)

    }
  }

}