// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  test("Genetic Encode |" ) {
    val text =  "|||||||||||||||||||||||"
    assertEquals(DogMath.geneticDecode(DogMath.geneticEncode(text)), text)
  } 
  test("Genetic Encode 0" ) {
    val text ="000000000000000000000000000"
    assertEquals(DogMath.geneticDecode(DogMath.geneticEncode(text)), text)
  } 
  test("Genetic Encode 1" ) {
    val text ="111111111111111111111111111"
    assertEquals(DogMath.geneticDecode(DogMath.geneticEncode(text)), text)
  }
}
