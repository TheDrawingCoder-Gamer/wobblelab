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
  def calculatedGenes: db.CalculatedGenes = masterGene.calculateGenes()(using db.DogContext(age))
}
