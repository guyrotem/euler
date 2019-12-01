package euler.problems

import euler.utils.EulerUtils

//  unsolved
object E401SumSquare extends App {

  def sigma2(n: Int) = {
    val primeFactors = EulerUtils.fullDecompose(n)
    val divisors = factorsToAllDivisors(primeFactors.toSeq)
    divisors.map(x => x * x).sum
  }

  private def factorsToAllDivisors(factors: Seq[(Int, Int)]): Seq[BigInt] = {
    factors.headOption map {
      case (div, count) => (0 to count) map {
        BigInt(div).pow
      } flatMap { mult =>
        factorsToAllDivisors(factors.tail) map {_ * mult}
      }
    } getOrElse Seq(1)
  }

  assert(sigma2(6) == 50)

  implicit class StreamUtils(stream: Stream[Int]) {
    private def plusMod(modBy: Int)(x: Int, y: Int): Int = {
      x % modBy + y % modBy
    }

    def sumMod(modBy: Int): Int = {
      stream.fold(0)(plusMod(modBy))
    }
  }

  implicit class StreamUtilsBig(stream: Stream[BigInt]) {
    private def plusMod(modBy: Int)(x: BigInt, y: BigInt): BigInt = {
      x % modBy + y % modBy
    }

    def sumMod(modBy: Int): BigInt = {
      stream.fold(BigInt(0))(plusMod(modBy))
    }
  }

  def sumOfSquares(n: BigInt) = {
    n * (n + 1) * (2 * n + 1) / 6
  }

  def sumOfSquaresBetween(from: BigInt, to: BigInt) = {
    sumOfSquares(to) - sumOfSquares(from - 1)
  }

  val modBy = 1000000000
  val max = BigInt("1000000000000000")

  def SIGMA2mod10e9Simple(max: BigInt) = {
    EulerUtils.naturals
      .takeWhile(_ <= max)
      .map(num => (((max / num) * num * num) % modBy).intValue)
      .sumMod(modBy)
  }

  def SIGMA2mod10e9Fast(max: BigInt) = {
    ???
//    divisors.zip(divisors.tail)
//      .map({case (prevDiv, div) =>
//        (max / div) * (sumOfSquares(div) - sumOfSquares(prevDiv))
//      })
//      .sum + max
  }

  //  println((1 to 6).map(BigInt.apply).map(SIGMA2mod10e9))
  println((1 to 8).map(sigma2))
  println((1 to 8).map(BigInt.apply).map(SIGMA2mod10e9Fast))
  assert((1 to 8).map(BigInt.apply).map(SIGMA2mod10e9Fast) == Seq(1, 6, 16, 37, 63, 113, 163, 248))
  //  println(SIGMA2mod10e9(BigInt("1000000000000000")))

}
