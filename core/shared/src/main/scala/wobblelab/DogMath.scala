package net.bulbyvr
package wobblelab

object DogMath {
  val seperatorSymbol = '|'
  val tolerance = 0.0001f
  val maxGeneLen = 20
  val geneticEncodeSequenceLen = 20
  val separatorSequenceSymbols = Vector(':', ';', '<', '=', '>', '?', '@', '[', '#', ']', '^', '_', '*')

  private def getEncodedCharForSeparatorSequence(input: String) = {
    input match {
      case "|00" => DogMath.separatorSequenceSymbols(0)
      case "|01" => DogMath.separatorSequenceSymbols(1)
      case "|10" => DogMath.separatorSequenceSymbols(2)
      case "|11" => DogMath.separatorSequenceSymbols(3)
      case "||0" => DogMath.separatorSequenceSymbols(4)
      case "||1" => DogMath.separatorSequenceSymbols(5)
      case "|0|" => DogMath.separatorSequenceSymbols(6)
      case "|1|" => DogMath.separatorSequenceSymbols(7)
      case "|||" => DogMath.separatorSequenceSymbols(8)
      case "|0" => DogMath.separatorSequenceSymbols(9)
      case "|1" => DogMath.separatorSequenceSymbols(10)
      case "||" => DogMath.separatorSequenceSymbols(11)
      case "|" => DogMath.separatorSequenceSymbols(12)
      case _ => '|'
    }
  }

  private def getSeparatorSequenceForEncodedChar(input: Char) = {
    input match {
      case ':' => "|00"
      case ';' => "|01"
      case '<' => "|10"
      case '=' => "|11"
      case '>' => "||0"
      case '?' => "||1"
      case '@' => "|0|"
      case '[' => "|1|"
      case '#' => "|||"
      case ']' => "|0"
      case '^' => "|1"
      case '_' => "||"
      case '*' => "|"
      case _ => "ERROR"
    }
  }

  private def interiorScramble(s: String, i: Int) = {
    val num = s.charAt((i + 1) % s.length())
    val num2 = (i + num) % s.length
    if (num == 1) then {
      // basically the same as num2 == (i + 1) % s.length
      // this is stupid {
      s
    } else {
      val c = s.charAt(i)
      val c2 = s.charAt(num2)
      val strBuilder = StringBuilder(s)
      strBuilder.setCharAt(i, c2).setCharAt(num2, c).toString()
    }
  }

  def scramble(input: String): String = {
    var text = input
    for (i <- 0 until input.length) {
      text = interiorScramble(text, i)
    }
    text
  }

  def unscramble(input: String): String = {
    var text = input
    for (i <- input.length - 1 to 0 by -1) {
      text = interiorScramble(text, i)
    }
    text
  }

  private def encodePrecedingZeros(input: String) = {
    if input.isEmpty then {
      ""
    } else {
      if input.indexOf('1') != -1 then {
        println("invalid call to encodePrecedingZeros")
        ""
      }
      else {
        val stepSize = 25
        var num2 = input.length
        var text = ""
        while (num2 > 0) {
          if (num2 > stepSize) {
            text :+= (96 + stepSize).toChar
          } else {
            text :+= (96 + num2).toChar
            num2 = 0
          }
        }
        String.valueOf(text.toArray)
      }
    }
  }

  def generateRandomGeneOfSize(size: Int): String = {
    (for (i <- 0 until size) yield {
      if (math.random() >= 0.5) {
        if (!(math.random() >= 0.95)) {
          '1'
        } else {
          '|'
        }
      } else {
        if (!(math.random() >= 0.95)) {
          '0'
        } else {
          '|'
        }
      }
    }).mkString("")
  }

  private def encodeLookup(input: String) = {
    if (input.isEmpty) {
      ""
    } else if (input.indexOf('1') == -1) {
      encodePrecedingZeros(input)
    } else {
      val (text, goodInput)
      = if (input.charAt(0) == '0') {
          val num = input.indexOf('1')
          (encodePrecedingZeros(input.substring(0, num)), input.substring(num, input.length))
        } else {
          ("", input)
        }
      text + Integer.toHexString(Integer.parseInt(goodInput, 2)).toUpperCase()
    }
  }

  private def decodeLookup(input: String) = {
    if input.isEmpty then {
      ""
    } else if input.length == 1 && input.charAt(0) >= 'a' && input.charAt(0) <= 'z' then {
      "0".repeat(input.charAt(0) - 97 + 1)
    } else {
      Integer.toBinaryString(Integer.parseInt(input, 16))
    }
  }// forgive me : (
  def geneticEncode(input: String): String = {
    var text = ""
    var text2 = ""
    var i = 0
    while (i < input.length) {
      if (input.charAt(i) == seperatorSymbol) {
        text += encodeLookup(text2)
        val num = if i + 3 < input.length then 3 else (3 - (i + 3 - input.length))
        text += getEncodedCharForSeparatorSequence(input.substring(i, i + num))
        i += num - 1
        text2 = ""
      } else {
        if (text2.nonEmpty && !text2.contains("1") && input.charAt(i) == '1') {
          text += encodePrecedingZeros(text2)
          text2 = ""
        }
        text2 += input.charAt(i)
        if (text2.length == geneticEncodeSequenceLen || i == input.length - 1) {
          text += encodeLookup(text2)
          text2 = ""
        } else if text2.length > geneticEncodeSequenceLen then {
          println("Invalid SUB length")
        }}
      i += 1
    }
    text
  }

  def geneticDecode(input: String): String = {
    val num = Math.floor(geneticEncodeSequenceLen / 4)
    var text = ""
    var text2 = ""
    for (i <- 0 until input.length) {
      if separatorSequenceSymbols.contains(input.charAt(i)) then {
        text += decodeLookup(text2)
        text += getSeparatorSequenceForEncodedChar(input.charAt(i))
        text2 = ""
      }
      else if input.charAt(i) >= 'a' && input.charAt(i) <= 'z' then {
        text += decodeLookup(text2)
        text += decodeLookup(input.charAt(i).toString)
        text2 = ""
      }
      else {
        text2 += input.charAt(i)
        if text2.length == num || i == input.length - 1 then {
          text += decodeLookup(text2)
          text2 = ""
        }
        else if (text2.length > num) then {
          println("Invalid SUB length")
        }}
    }
    text
  }
  def getFloatFromBinaryString(binary: String): Float = java.lang.Long.parseLong(binary, 2)
  def getNumBinaryPermutations(geneLen: Int): Int = {
    val num = Math.round(Math.pow(2, geneLen) - 1).toInt
    if (num < 0) {
      Integer.MAX_VALUE
    } else {
      num
    }
  }
  def getFloatFromGeneSequence(sequence: String, minVal: Float, maxVal: Float): Float = {
    val gene = sequence.replace(seperatorSymbol.toString, "")
    if (gene.length > maxGeneLen) {
      return getFloatFromGeneSequence(gene.substring(0, maxGeneLen), minVal, maxVal)
    }
    if (gene.isEmpty) {
      return minVal
    }
    val num = getNumBinaryPermutations(gene.length)
    val num2: Float = getFloatFromBinaryString(gene) / num
    val num3 = maxVal - minVal
    val num4 = num2 * num3 + minVal
    val num5 = 0.1f
    if (num4 - num5 > maxVal) {
      maxVal
    } else if (num4 + num5 < minVal) {
      minVal
    } else {
      num4
    }

  }
}
