package net.bulbyvr
package wobblelab

case class GameDog(dogName: String,
                   masterGene: db.MasterDogGene,
                   personality: DogPersonality,
                   age: DogAge,
                   ageProgress: Float,
                   eolModifier: Float,
                   lifeExtension: Float) {
  def asRawDog: RawDog = {
    val rawGene = masterGene.getRawString
    RawDog(GeneVersion.Three, rawGene.dogGene, rawGene.domRecGene, age, ageProgress, eolModifier, lifeExtension, personality, dogName)
  }

  def updatedPercent(gene: db.Gene, value: Float): cats.data.ValidatedNec[String, GameDog] =
    println(value)
    masterGene.updatedPercent(gene, value)(using db.DogContext(age)).map(it => copy(masterGene = it))

  def withDogCtx[T](f: db.DogContext ?=> T): T =
    f(using db.DogContext(age))

  def calculatedGenes: db.CalculatedGenes = masterGene.calculateGenes()(using db.DogContext(age))
}
