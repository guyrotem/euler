package euler.problems

import euler.utils.EulerUtils

/**
  * Created on 25/07/2016.
  */
object E187SemiPrimes extends App {
  val twoPlus = EulerUtils.naturals.tail

  def descendingNaturals(startingAt: Int): Stream[Int] = {
    startingAt #:: descendingNaturals(startingAt - 1)
  }

  def findMinComposite(num: Int): Option[Int] = {
    (2 to (Math.sqrt(num).toInt + 1)) find { x =>
      num % x == 0
    }
  }

  def isSemiPrime(num: Int): Boolean = {
    findMinComposite(num) exists { comp =>
      EulerUtils.sqrtPrimeCheck(num / comp)
    }
  }

  assert(isSemiPrime(4))
  assert(!isSemiPrime(5))
  assert(isSemiPrime(6))
  assert(!isSemiPrime(7))
  assert(!isSemiPrime(8))
  assert(isSemiPrime(9))
  assert(isSemiPrime(10))
  assert(isSemiPrime(14))
  assert(!isSemiPrime(30))

//  val semiPrimesCount = (4 to 100000000) count isSemiPrime


//  val semiPrimesStream = sieve(Stream(2))
//  println(semiPrimesStream.take(20).toList)
}
