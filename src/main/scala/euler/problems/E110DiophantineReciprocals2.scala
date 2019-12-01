package euler.problems

import euler.utils.EulerUtils
import scala.language.postfixOps

/**
  * Created on 09/09/2016.
  */
object E110DiophantineReciprocals2 extends App {
  def numberOfSplits(n: BigInt) = {
    val divisorsCountOfNCube = EulerUtils.fullDecomposeBig(n).toSeq.map {
      case (_, x) => 2 * x + 1
    }
    (divisorsCountOfNCube.product + 1) / 2
  }

  assert(numberOfSplits(1260) == 113)

  val primes = EulerUtils.naturalsStartingAt(2) filter EulerUtils.sqrtPrimeCheck
  val primesProduct: Stream[BigInt] = BigInt(2) #:: ((primes.tail zip primesProduct) map {case (x, y) => x * y})
  val series = (primesProduct zip primes.tail) flatMap {case (x, p) =>
    (1 until p) map {_ * x}
  }

  val numberOfSolutions = series map numberOfSplits
  println((series zip numberOfSolutions).find(x => x._2 > 4000000))

  def getMaxSeries(f: BigInt => BigInt): Stream[Int] = {
    val values = EulerUtils.bigNaturals.map(f).zipWithIndex
    lazy val outStream: Stream[(BigInt, Int)] = values.head #:: ((values.tail zip outStream) map {case(valWithIndex, lastMax) =>
      findItemBiggerThan(lastMax._1, values.drop(valWithIndex._2))
    })
    outStream map {_._2 + 1}
  }

  def findItemBiggerThan(x: BigInt, s: Stream[(BigInt, Int)]) = {
    (s find {case (v, _) => v > x}).get
  }

  println(series take 25 toList)
  println(getMaxSeries(numberOfSplits).take(25).toList)
}
