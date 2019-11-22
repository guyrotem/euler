package euler.problems

import euler.utils.EulerUtils

/**
  * Created on 16/07/2016.
  */
object CircularPrimes extends App {
  def generateAllCycles(num: Int): Seq[Int] = {
    val numString = num.toString
    (0 until numString.length) map { x =>
      (numString.substring(x) + numString.substring(0, x)).toInt
    }
  }

  def isCircularPrime(num: Int): Boolean = {
    generateAllCycles(num) forall EulerUtils.sqrtPrimeCheck
  }

  assert(isCircularPrime(7))
  assert(isCircularPrime(197))
  assert(!isCircularPrime(101))

  println((2 to 999999) count isCircularPrime)
}
