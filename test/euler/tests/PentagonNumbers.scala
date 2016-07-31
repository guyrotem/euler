package euler.tests

import euler.utils.EulerUtils

/**
  * Created on 23/07/2016.
  */
object PentagonNumbers extends App {
  def getNthPentagonal(n: BigInt) = n * (3*n - 1) / 2

//  3n^2 - n - 2m = 0
  def isPentagonal(m: BigInt): Boolean = {
    isSquare(1+24 * m) && (((1 + Math.sqrt(1+24*m.toDouble)).toInt % 6) == 0)
  }

  def pentagonalDiff(index: Int, gap: Int) = gap * (6*index + 3*gap - 1) / 2
  def pentagonalSum(index: Int, gap: Int) = getNthPentagonal(index) + getNthPentagonal(index + gap)

  assert(pentagonalDiff(1, 1) == 4)
  assert(pentagonalDiff(2, 1) == 7)
  assert(pentagonalDiff(1, 2) == 11)
  assert(pentagonalDiff(1, 3) == 21)
  assert(pentagonalDiff(3, 4) == 58)

  assert(pentagonalSum(1, 1) == 6)
  assert(pentagonalSum(3, 4) == 82)

  def isSquare(num: BigInt): Boolean = {
    val sq = Math.sqrt(num.toDouble).toInt
    sq * sq == num
  }

//  val pentagonals = EulerUtils.bigNaturals map getNthPentagonal

  val generate = for {
    i <- 1 to 2000
    j <- 1 to 2000 if isPentagonal(pentagonalDiff(i, j)) && isPentagonal(pentagonalSum(i, j))
  } yield (i, j)

  println(generate.head)
  assert(isPentagonal(getNthPentagonal(generate.head._2 + generate.head._1) - getNthPentagonal(generate.head._1)))
  println(getNthPentagonal(generate.head._2 + generate.head._1) - getNthPentagonal(generate.head._1))
}