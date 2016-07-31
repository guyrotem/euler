package euler.tests

import euler.utils.{EulerTestsBase, EulerUtils}

/**
  * Created on 31/07/2016.
  */
object E162HexaNumbers extends EulerTestsBase {
//  val digits13 = 13 * BigInt(13).pow(15)
//  val digits14 = 2 * (14 * BigInt(14).pow(15) - BigInt(13).pow(16)) + 13 * (BigInt(14).pow(15) - BigInt(13).pow(15))
//  val digits15 = BigInt(15).pow(16) - BigInt(13).pow(16) + 2 * (14 * BigInt(15).pow(16) - BigInt(13).pow(16))
//  val digits16 = 15 * BigInt(16).pow(15)
//
//  println(digits16 / digits13)
//
//  val digits1 = 15 * BigInt(16).pow(3)
//  val digits2 = 15 * BigInt(16).pow(3)
//  val digits3 = 15 * BigInt(16).pow(2)
//  val digits4 = 15 * BigInt(16).pow(3)

//  val result = digits16 - digits15 - digits14 - digits13

  private def pow(base: Int, exp: Int): BigInt = {
    BigInt(base).pow(exp)
  }

//  println(result)
//  println(convertToHexa(result))
//  println(EulerUtils.decomposeBig(result))

  def convertToHexa(num: BigInt): String = {
    val BASE = 16

    def getAsChar(c: Int): String = {
      c match {
        case 0 => "0"
        case 1 => "1"
        case 2 => "2"
        case 3 => "3"
        case 4 => "4"
        case 5 => "5"
        case 6 => "6"
        case 7 => "7"
        case 8 => "8"
        case 9 => "9"
        case 10 => "A"
        case 11 => "B"
        case 12 => "C"
        case 13 => "D"
        case 14 => "E"
        case 15 => "F"
      }
    }

    var nextInt = num
    val str: StringBuilder = new StringBuilder

    while (nextInt > 0) {
      val nextDigit = nextInt % BASE
      str.append(getAsChar(nextDigit.toInt))
      nextInt /= 16
    }

    str.toString().reverse
  }

  assert(convertToHexa(BigInt("109056873337020")) == "632FC83210BC")

  private def generateAll(numDigits: Int): Seq[String] = {
    (pow(2, 4 * (numDigits - 1)) to (pow(2, 4 * numDigits) - 1)) map convertToHexa
  }

  private def countBruteForce(numDigits: Int): Int = {
    generateAll(numDigits) count { x => x.contains("A") && x.contains("1") && x.contains("0") }
  }

  def countPerLength(numDigits: Int): BigInt = {
    def help(numDigits: Int, missing: Int): BigInt = {
      if (numDigits < missing) 0
      else if (missing == 0 && numDigits == 0) 1
      else if (missing == 0) pow(16, numDigits)
      else (16 - missing)*help(numDigits - 1, missing) + missing*help(numDigits - 1, missing - 1)
    }
    (16 - 3) * help(numDigits - 1, 3) + 2 * help(numDigits - 1, 2)
  }

  assert(countPerLength(3) == countBruteForce(3))
  assert(countPerLength(4) == 258)
  assert(countPerLength(5) == 9928)
//  assert(countPerLength(6) == countBruteForce(6))
//  println(generateAll(2))

  val result = (3 to 16) map countPerLength sum

  println(convertToHexa(result))

}
