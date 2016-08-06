package euler.tests

import euler.utils.EulerUtils

/**
  * Created on 05/08/2016.
  */
object E211SquareDivisors extends App {
  def sumSquareFactors(num: Int): BigInt = {
    val factors = EulerUtils.fullDecompose(num)
    val divisors = help(factors.toSeq)
    (divisors map {x => BigInt(x) * BigInt(x)}).sum
  }

  private def help(factors: Seq[(Int, Int)]): Seq[Int] = {
    factors match {
      case Seq((div, count), xs@_*) => (0 to count) flatMap { pow =>
        help(xs) map {_ * Math.pow(div, pow).toInt}
      }
      case Seq() => Seq(1)
    }
  }

  assert(sumSquareFactors(10) == 130, sumSquareFactors(10))

  def isSumSquareDivisors(num: Int): Boolean = {
    val ssf = sumSquareFactors(num)
    val root = Math.sqrt(ssf.toDouble).toInt
    BigInt(root) * BigInt(root) == ssf
  }

  println((1 to 1234567) filter isSumSquareDivisors)
}
