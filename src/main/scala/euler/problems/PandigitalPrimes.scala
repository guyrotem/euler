package euler.problems

import euler.utils.EulerUtils

/**
  * Created on 23/07/2016.
  */
object PandigitalPrimes extends App {
  def isNPanDigital(digits: Int): Boolean = {
    isNPanDigital(EulerUtils.splitToDigits(digits))
  }

  private def isNPanDigital(digits: Seq[Int]): Boolean = {
    (1 to digits.length) forall { digits contains _ }
  }

  val panDigs = (1234567 to 7654321).reverse.toStream filter isNPanDigital
  val panDigPrimes = panDigs filter EulerUtils.sqrtPrimeCheck
  //7652413
  //  NOTE: 8&9 digits nPanDig numbers divide by 3, therefore cannot be primes

  println(panDigPrimes.head)
}
