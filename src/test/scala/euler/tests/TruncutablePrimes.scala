package euler.tests

import euler.utils.EulerUtils

/**
  * Created on 22/07/2016.
  */
object TruncutablePrimes extends App {
  def isTruncutablePrime(prime: Int): Boolean = {
    val leftTrunc = ((1 until prime.toString.length) map {prime.toString.substring(_).toInt}) forall EulerUtils.sqrtPrimeCheck
    val rightTrunc = ((1 until prime.toString.length) map { x => prime.toString.substring(0, prime.toString.length - x).toInt}) forall EulerUtils.sqrtPrimeCheck
    leftTrunc && rightTrunc
  }
  val primes11Plus = EulerUtils.naturalsStartingAt(11) filter EulerUtils.sqrtPrimeCheck
  val truncutablePrimes = primes11Plus filter isTruncutablePrime
  println(truncutablePrimes.take(11).toList.sum)
}

