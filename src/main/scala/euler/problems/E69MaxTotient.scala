package euler.problems

import euler.utils.EulerUtils
import scala.language.postfixOps

/**
  * Created on 04/09/2016.
  */
object E69MaxTotient extends App {
  val primes = EulerUtils.naturals.tail filter EulerUtils.sqrtPrimeCheck
  val primesMult: Stream[Int] = 1 #:: ((primes zip primesMult) map { case (x, y) => x * y })

  println((primesMult zip primesMult.tail) collect {case (x, y) if y > 1000000 => x} head)
  // 510510
}
