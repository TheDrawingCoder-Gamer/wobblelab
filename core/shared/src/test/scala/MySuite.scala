import net.bulbyvr.wobblelab.*


class MySuite extends munit.FunSuite {

  val testDog = "8=F0D2^370811cUF64:7A141.A0Ybba88^6a1018F1aA;8208CPaA9690Pagg8CFC0EEA2F;0195c;F280F8812F3D88bDC2eAdB29AAcDa424D^;9cEe44:252<EC:8b7E25aaa;8:00c:E:3a2:712;1:42;ab:=cAFE2a7121<E6F:61:484108^6bF:EF^F074B3^BF^FaCdADBbrf1CPdC802C3F8F532^0163a90301561;6rCAFC1C0CC8A00At694DD4F9<074aE60^C<85Db50=b0^9ECF0^34B05E;h03;2Be91c"
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
    (0 until 100).foreach: _ =>
      val text = DogMath.generateRandomGeneOfSize(10);
      assertEquals(DogMath.geneticDecode(DogMath.geneticEncode(text)), text)
  }
  test("Random Gene 100 length") {
    (0 until 100).foreach: _ =>
      val text = DogMath.generateRandomGeneOfSize(100);
      assertEquals(DogMath.geneticDecode(DogMath.geneticEncode(text)), text)
  }
  test("Random Gene 500 length") {
    (0 until 100).foreach: _ =>
      val text = DogMath.generateRandomGeneOfSize(500);
      assertEquals(DogMath.geneticDecode(DogMath.geneticEncode(text)), text)
  }
  test("Random Gene 1000 length") {
    (0 until 100).foreach: _ =>
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

  test("DogMath dynamic float Plus minus") {
    val x = 0.5f
    val y = DogMath.dynamicFloatToGeneSequence(db.GeneticProperty.BodyScaleXPlus, x, 0, db.GeneticProperty.BodyScaleX.maxBound).get
    println(y)
    val z = DogMath.getDynamicFloatFromSequence(db.GeneticProperty.BodyScaleXPlus, y, 0, db.GeneticProperty.BodyScaleX.maxBound).get
    assertEquals(x, z)
  }
}
