package euler.tests

import euler.utils.EulerUtils

/**
  * Created on 23/07/2016.
  */
object CoResilience extends App {

  def ci(n: BigInt): (BigInt, BigInt) = {
    (n - numResilient(n),
      n - 1)
  }

  def isUnit(nd: (BigInt, BigInt)): Boolean = nd._2 % nd._1 == 0

  def numResilient(n: BigInt): BigInt = {
    //  see TOTIENT FUNCTION
    val primeFactors = EulerUtils.fullDecomposeBig(n).keys
    n * (primeFactors map {_ - 1}).product / primeFactors.product
  }

  def resilience(d: BigInt) = {
    numResilient(d).toDouble / (d - 1).toDouble
  }

//  println((BigInt(3) to 100) map {num => (ci(num), isUnit(ci(num)))})

  val relevantStream = EulerUtils.bigNaturals.map {2*_+1} filter { num =>
    val cival = ci(num)
    cival._1 != 1 && isUnit(cival)
  }

//  println(relevantStream.take(50).toList)
  val firstN = List(15, 85, 255, 259, 391, 589, 1111, 3193, 4171, 4369, 12361, 17473, 21845, 25429, 28243, 47989, 52537, 65535, 65641, 68377, 83767, 91759, 100777, 120019, 144097, 167743, 186367, 268321, 286357, 291919, 316171, 327937, 335923, 346063, 353029, 360301, 404797, 406867, 524851, 531721, 558013, 563767, 633727, 705667, 738607, 910489, 970141, 1013539, 1080769, 1093987)
  val diffN = (firstN zip firstN.tail) map {x => x._2 - x._1}
//  println(diff20)
  println(firstN map EulerUtils.fullDecompose)

  println((firstN map EulerUtils.fullDecompose) map {_.keys.map({_-1}).product})
  //  we see that: a number N is coresilient iff: it can be decomposed into primes (1st deg) where:
  //  ( N - PI[i](p-1) ) divides (N-1) <=> (PI{p} - PI{p-1}) divides (N-1)
  //  2 primes: (p1 + p2 - 1) divides (p1*p2 - 1)
  //  3 primes: p1p2p3 - (p1 - 1)(p2 - 1)(p3 - 1) divides (p1p2p3 - 1) <=> (p1p2+p2p3+p1p3 - (p1+p2+p3) + 1) divs (p1p2p3-1)
}
