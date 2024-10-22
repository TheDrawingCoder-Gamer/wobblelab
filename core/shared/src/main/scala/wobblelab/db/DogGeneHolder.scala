package net.bulbyvr
package wobblelab.db

sealed trait DogGeneHolder

object DogGeneHolder {
  case class Standard(geneString: String) extends DogGeneHolder
  case class Super(geneString: String, originalLength: Int, maxValIncrease: Float) extends DogGeneHolder
  case class Looped(rawGene: String, length: Int, discrete: Boolean) extends DogGeneHolder {
    def iterator: Iterator[String] = rawGene.grouped(length)
  }
}