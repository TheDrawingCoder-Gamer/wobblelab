import net.bulbyvr.wobblelab.*


class MySuite extends munit.FunSuite {

  val testDog = "4CaFb3881Ca;2F8Db58b0c10:^F370E4U^C=3610:0EdF3f0D3<tn89B660c0C2A2^00210A4346a:F4B0c12068028D81160D8Abb5^69F83Cc720724=8;;:g6C2dC4=ba:b01c210b9C1429c^F<B013^4a08F=;B67;<8aF0;9aF23<=7b0FD:8FFFF^:b8Ad3F2ED7=03CF14a3^7EF;FC1T0^A63.002L03b6=01110<0ag1^1C104C:5002Cc90b92c;F1C8E638a2C84DFdE186DD09F3a69.1bcc21Ci13^7Fca1:0"
  test ("Scramble Gene") {
    val text = "FD<b61CF801540a5E8421EB:Fa2E3961D7;110:02=:040B2<d6FoE0FF1YF3^8EF990a0lb3F1a8F618E0d0=d2034C20;Ba0^1;99bBCBB2aFECa2<1b1:1<b8DE81AC<8<8b:882;6c=8Aa4;68337:C34:Fd:1;Caa40ac;E84b6c2B1^849P0FAfFaB557F2033DCF018^FFF^P8aD3P<AqF93D562.E68:77^128.00^51209cer02^4590FBA0ABEFT7uE80FB852EC0^4FF9EFA715UDD8E;91Eabb96^47b51<AE3:Dc^c"
    assertEquals(DogMath.scramble(DogMath.unscramble(text)), text)
  }
  test ("more scramble") {
    assertEquals(DogMath.scramble(DogMath.unscramble(testDog)), testDog)
  }
  test ("raw dog inout") {
    val scrambledRes = DogRegistry.exportDog(DogRegistry.importDog(testDog).get)
    assertEquals(scrambledRes, testDog)
  }
  test ("game dog io") {
    val rawdoggie = DogRegistry.importDog(testDog).get
    val doggie = rawdoggie.toGameDog
    val rerawedDoggie = doggie.asRawDog
    // domRecGene will be different because jank
    assertEquals(rerawedDoggie.dogGene, rawdoggie.dogGene)
  }
  
  test ("Full I/O") {
    val rawdoggie = DogRegistry.importDog(testDog).get
    val doggie = rawdoggie.toDog
    val reraweddoggie = doggie.asRawDog.get
    assertEquals(reraweddoggie, rawdoggie)
    val outText = DogRegistry.exportDog(reraweddoggie)
    assertEquals(outText, testDog)
  }
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
  test ("Random Gene 10 length") {
    val text = DogMath.generateRandomGeneOfSize(10);
    assertEquals(DogMath.geneticDecode(DogMath.geneticEncode(text)), text)
  }
  test("Random Gene 100 length") {
    val text = DogMath.generateRandomGeneOfSize(100);
    assertEquals(DogMath.geneticDecode(DogMath.geneticEncode(text)), text)
  }
  test("Random Gene 500 length") {
    val text = DogMath.generateRandomGeneOfSize(500);
    assertEquals(DogMath.geneticDecode(DogMath.geneticEncode(text)), text)
  }
  test("Random Gene 1000 length") {
    val text = DogMath.generateRandomGeneOfSize(1000);
    assertEquals(DogMath.geneticDecode(DogMath.geneticEncode(text)), text)
  }
  test ("Roundtrip Master Gene") {
    // May not hold up in general case (for arbitrary inputs where dom rec genes can be right het, like aA)
    val dog = Dog.randy.asRawDog.get.toGameDog
    val x = DogRegistry.exportDog(dog.asRawDog)
    val y = DogRegistry.exportDog(Dog.randy.asRawDog.get)
    assertEquals(x, y)
  }
}
