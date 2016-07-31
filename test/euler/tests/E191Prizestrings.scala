package euler.tests

import euler.utils.{EulerTestsBase, EulerUtils}

/**
  * Created on 30/07/2016.
  */
object E191PrizeStrings extends EulerTestsBase {

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

  eulerAssertion(numberOfPrizeString(4), 43)
  eulerAssertion(numberOfPrizeString(5), 94)
  eulerAssertion(numberOfPrizeString(6), 200)
  eulerAssertion(numberOfPrizeString(7), 418)

  eulerAssertion(generateAllStrings(2), Seq("AA", "AO", "OA", "OO"))
  eulerAssertion(generateAllStrings(3).length, 8)

  eulerAssertion(bruteForceNumberOfPrizeString(4), 43)
  eulerAssertion(bruteForceNumberOfPrizeString(5), 94)
  eulerAssertion(bruteForceNumberOfPrizeString(6), 200)
  eulerAssertion(bruteForceNumberOfPrizeString(7), 418)

  println(numberOfPrizeString(30))
  //1918080160
}
