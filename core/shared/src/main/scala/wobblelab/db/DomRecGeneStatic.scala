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

given domRecGeneDecoder: Decoder[DomRecGeneStatic]
  = Decoder.forProduct5("version", "AA", "Aa", "aa", "defaultValue")(DomRecGeneStatic.apply)
given domRecGeneEncoder: Encoder[DomRecGeneStatic] = {
  Encoder.forProduct5("version", "AA", "Aa", "aa", "defaultValue") { g =>
    (g.version, g.dom, g.het, g.sub, g.defaultValue)
  }
}
