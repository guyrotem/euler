package euler.tests

import euler.utils.EulerUtils
import scala.language.implicitConversions

/**
  * Created on 06/08/2016.
  */
object E313Sliding extends App {
  def solveMN(m: BigInt, n: BigInt) = {
    assert(m >= n)

    if (m-n > 1) 6*m + 2*n - 13
    else 4 * (m + n) - 11
  }

  def getMaxM(n: BigInt, maxP: BigInt) = {
    (maxP + 13 - 2 * n) / 6 + 1
  }

  assert(solveMN(2, 2) == 5)
  assert(solveMN(5, 4) == 25)


  assert(solveMN(3, 2) == 9)
  assert(solveMN(4, 2) == 15, solveMN(4, 2))
  assert(solveMN(5, 2) == 21)

  val maxP = BigInt("1000000000000")
  val maxN = BigInt("125000000003")

  println(solveMN(maxN, maxN))

  var sum = 0
  var n = BigInt(2)

//
//  while (n < maxN) {
//    var m = n
//    while (m < getMaxM(n, maxP)) {
//      val sol = solveMN(m, n)
//      val sqrt = EulerUtils.bigIntSqRootCeil(sol)
//      if (sqrt * sqrt == sol && EulerUtils.sqrtPrimeCheck(sqrt.toInt))
//        sum += 1
//      m += 1
//    }
//    n += 1
//  }

  println(sum)



}
