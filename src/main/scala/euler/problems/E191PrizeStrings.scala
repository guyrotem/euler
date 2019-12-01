package euler.problems

import euler.utils.EulerUtils

import scala.language.postfixOps

object E191PrizeStrings {

  def numberOfPrizeString(stringLength: Int): BigInt = {
    //  SUM[ (OA arrangements) * (#O + 1) ]
    val optionsPerACount = (0 to stringLength) map {aCount => numberOfOptions(stringLength, aCount) * (stringLength + 1 - aCount)}
    optionsPerACount.sum
  }

  private def generateAllStrings(stringLength: Int): Seq[String] = {
    if (stringLength == 0) Seq("")
    else Seq("A", "O") flatMap {c => generateAllStrings(stringLength - 1) map {s => c + s}}
  }

  def bruteForceNumberOfPrizeString(stringLength: Int): Int = {
    val allOptions: Seq[String] = generateAllStrings(stringLength)
    allOptions filterNot {_.contains("AAA")} map {1 + _.count({_ == "O".charAt(0)})} sum
  }

  private def numberOfOptions(totalLength: Int, numberOfAs: Int): BigInt = {
    numberOfAs match {
      case 0 => 1
      case 1 => totalLength
      case x if x <= 2*(totalLength+1)/3 =>
        numberOfOptions(totalLength - 1, numberOfAs) +
        numberOfOptions(totalLength - 2, numberOfAs - 1) +
        numberOfOptions(totalLength - 3, numberOfAs - 2)
      case _ => 0
    }
  }

  private def pick(n: BigInt, k: BigInt): BigInt = {
    EulerUtils.factorial(n) / EulerUtils.factorial(k) / EulerUtils.factorial(n - k)
  }

  assume(numberOfPrizeString(4) == 43)
  assume(numberOfPrizeString(5) == 94)
  assume(numberOfPrizeString(6) == 200)
  assume(numberOfPrizeString(7) == 418)

  assume(generateAllStrings(2) == Seq("AA", "AO", "OA", "OO"))
  assume(generateAllStrings(3).length == 8)

  assume(bruteForceNumberOfPrizeString(4) == 43)
  assume(bruteForceNumberOfPrizeString(5) == 94)
  assume(bruteForceNumberOfPrizeString(6) == 200)
  assume(bruteForceNumberOfPrizeString(7) == 418)

  println(numberOfPrizeString(30))
  //1918080160
}
