package euler.tests

import euler.utils.EulerUtils

/**
  * Created on 23/07/2016.
  */
object TrianglePentagonHexagon extends App {
  def isTriangleNumber(num: Int): Boolean = {
    //  n^2 + n - 2*num = 0
    //  n = -1 ± sqrt(1 + 8*num) / 2 = [(sqrt(1 + 8*num) - 1) / 2]
    val nApprox = (0.5 * (Math.sqrt(1 + 8 * num) - 1)).toInt
    nApprox * (nApprox + 1) / 2 == num
  }

  def getNthPentagonal(n: BigInt) = n * (3*n - 1) / 2

  def isPentagonNumber(num: BigInt): Boolean = {
    // 3n^2 − n - 2*num == 0
    val nApprox = (1 + bigSqrt(1 + 24 * num)) / 6
    nApprox * (3*nApprox - 1) / 2 == num
  }

  def bigSqrt(num: BigInt): BigInt = {
    BigInt(Math.sqrt(num.toDouble).round.toString)
  }

  def isHexagonNumber(num: BigInt): Boolean = {
    // 2n^2 − n - num == 0
    val nApprox = (1 + bigSqrt(1 + 8 * num)) / 4
    nApprox * (2*nApprox - 1) == num
  }

  assert(isTriangleNumber(40755))
  assert(isPentagonNumber(40755))
  assert(isHexagonNumber(40755))

  val tph = EulerUtils.bigNaturals.tail map getNthPentagonal filter {x => isPentagonNumber(x) && isHexagonNumber(x)}
  println(tph(1))
  //1533776805
}
