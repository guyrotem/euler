package euler.tests

import euler.utils.{EulerTestsBase, EulerUtils}

/**
  * Created on 30/07/2016.
  */
object E183MaximumProductOfParts extends EulerTestsBase {

  def wrapper(n: Int): Int = {
    val k = productOfPartsBruteForce(n)
    if (isTerm(n, k)) -n else n
  }

  def productOfPartsBruteForce(n: Int): Int = {
    //  fails for n > 1929 due to decimal calcs limitations
    val res = 1 until n map productOfParts(n)
    val indexResult = res.indexOf(res.max) + 1
    indexResult
  }

  def wrapperQuick(n: Int): Int = {
    val k = productOfPartsQuick(n)
    if (isTerm(n, k)) -n else n
  }

  def productOfPartsQuick(n: Int): Long = Math.round(n / Math.E)

  private def isTerm(n: Int, k: Long): Boolean = {
    val (n2, k2) = EulerUtils.shrinkNumbersBig(n, k)
    EulerUtils.decomposeBig(k2) forall {x => x ==2 || x ==5}
  }

  private def productOfParts(n: Int)(k: Int): Double = {
    //  NOTE: overflows for n~1000, k~177 and bigger...
    Math.pow(n.toDouble / k, k)
  }

  eulerAssertion(productOfPartsBruteForce(11), 4)
  eulerAssertion(productOfPartsBruteForce(8), 3)

  eulerAssertion(productOfPartsQuick(11), 4)
  eulerAssertion(productOfPartsQuick(8), 3)

  eulerAssertion(isTerm(312, 125), true)
  eulerAssertion(isTerm(512, 375), false)
  eulerAssertion(isTerm(3 * 7 * 101 * 37 * 41, 3 * 7 * 2 * 2 * 5 * 5 * 5), true)

  (5 to 1929) foreach { n =>
    eulerAssertion(productOfPartsBruteForce(n), productOfPartsQuick(n))
  }

  eulerAssertion((5 to 100) map wrapper sum, 2438)
  eulerAssertion((5 to 100) map wrapperQuick sum, 2438)

  //  48861552
  println((5 to 10000) map wrapperQuick sum)


}
