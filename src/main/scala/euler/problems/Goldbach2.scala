package euler.problems

import euler.utils.EulerUtils

/**
  * Created on 23/07/2016.
  */
object Goldbach2 {
  val primes = EulerUtils.naturals filter EulerUtils.sqrtPrimeCheck
  val squares = EulerUtils.naturals map {x => x * x}

  def canDecompose(num: Int): Boolean = {
    squares.take(num) exists { sq =>
      EulerUtils.sqrtPrimeCheck(num - 2 * sq)
    }
  }

  assert(canDecompose(9))
  assert(!canDecompose(32))
  assert(canDecompose(33))

  val compositeOdds = EulerUtils.naturals map { 2 * _ + 1 } filter {!EulerUtils.sqrtPrimeCheck(_)}

  println(compositeOdds find {!canDecompose(_)})
  //5777

}
