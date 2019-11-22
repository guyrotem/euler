package euler.problems

import euler.utils.EulerUtils

/**
  * Created on 05/08/2016.
  */
object E211DivisorSquareSum extends App {
  def sumSquareFactors(num: Int): BigInt = {
    val factors = EulerUtils.fullDecompose(num)
    val divisors = help(factors.toSeq)
    sumSquare(divisors)
  }

  private def sumSquare(nums: Seq[Int]): BigInt = {
    (nums map {x => BigInt(x) * BigInt(x)}).sum
  }

  private def help(factors: Seq[(Int, Int)]): Seq[Int] = {
    factors.headOption map {
      case (div, count) => (0 to count) map {
        Math.pow(div, _).toInt
      } flatMap { mult =>
        help(factors.tail) map {_ * mult}
      }
    } getOrElse Seq(1)
  }

  assert(sumSquareFactors(10) == 130, sumSquareFactors(10))

  def isSumSquareDivisors(num: Int): Boolean = {
    val ssf = sumSquareFactors(num)
    isPerfectSquare(ssf)
  }

  def isPerfectSquare(x: BigInt): Boolean = {
    val root = Math.sqrt(x.toDouble).toInt
    BigInt(root) * BigInt(root) == x
  }

  println((1 to 1234567) filter isSumSquareDivisors)
}
