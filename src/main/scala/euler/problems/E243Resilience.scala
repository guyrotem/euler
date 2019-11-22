package euler.problems

import euler.utils.EulerUtils

/**
  * Created on 23/07/2016.
  */
object E243Resilience extends App {

  def numResilientNaive(d: BigInt) = {
    1 + ((BigInt(2) until d) count {EulerUtils.isBigCoPrime(_, d)})
  }

  def numResilient(n: BigInt): BigInt = {
    //  see TOTIENT FUNCTION
    val primeFactors = EulerUtils.fullDecomposeBig(n).keys
    n * (primeFactors map {_ - 1}).product / primeFactors.product
  }

  def resilience(d: BigInt) = {
    numResilient(d).toDouble / (d - 1).toDouble
  }

  println(numResilient(12))
  assert(numResilient(12) == 4)
  assert(resilience(12) == 4.0/11)

  assert(numResilientNaive(30) == 8)
  println(numResilient(30))
  assert(numResilient(30) == 8)

  def isResilientBelowThreshold(d: Int, threshold: Double): Boolean = {
    resilience(d) < threshold
  }

  //  1st attempt
  println(EulerUtils.naturals.tail find {x => isResilientBelowThreshold(x, 0.25)})

  //  get naive stream of minimums
  val minResStreamNaive: Stream[Int] = 1 #:: (minResStreamNaive map {y => (EulerUtils.naturalsStartingAt(y + 1) find {x => isResilientBelowThreshold(x, resilience(y))}).get})

  case class MinResilient(currentMin: BigInt, currentResilience: Double, lastDiff: BigInt, primeStream: Stream[Int]) {
    def nextResilient: MinResilient = {
      if (resilience(currentMin + lastDiff) < currentResilience)
        MinResilient(currentMin + lastDiff, resilience(currentMin + lastDiff), lastDiff, primeStream)
      else if (resilience(currentMin + lastDiff * primeStream.head) < currentResilience)
        MinResilient(currentMin + lastDiff * primeStream.head, resilience(currentMin + lastDiff * primeStream.head), lastDiff * primeStream.head, primeStream.tail)
      else throw new Exception(s"Couldn't find more: $currentMin")
    }
  }

  val firstRes = MinResilient(1, Double.PositiveInfinity, 1, EulerUtils.naturals filter EulerUtils.sqrtPrimeCheck)
  println(firstRes)

  //  2nd attempt: there's some prime-related pattern to these minima
  val minResilientStream: Stream[MinResilient] = firstRes #:: (minResilientStream map {_.nextResilient})

  //  pattern is discovered! generate a descending series of minima.
  //    "varying" arithmetic sequence: gaps are sum of all n primes: PI[1 to n](Pr[i])
  //    each "gap" is repeated Pr[i] - 1 times.
  val result = {
    def createRepeatedSeq[T](diffsAndCount: (T, Int)): Seq[T] = {
      List.fill(diffsAndCount._2)(diffsAndCount._1)
    }

    val primes: Stream[Int] = EulerUtils.naturals filter EulerUtils.sqrtPrimeCheck
    val numDuplicates: Stream[Int] = primes map {_ - 1}
    lazy val diffs: Stream[BigInt] = 1 #:: (diffs zip primes map {case (a,b) => a * b})
    val actualDiffs: Stream[BigInt] = (diffs zip numDuplicates) flatMap createRepeatedSeq
    lazy val actualSeries: Stream[BigInt] = 1 #:: (actualSeries zip actualDiffs map {case(a, b) => a + b})

    println(numDuplicates.take(10).toList)
    println(diffs.take(10).toList)
    println(actualDiffs.take(15).toList)
    actualSeries
  }

  //  Check resilience using Euler's Totient function (not brute force)
  println(result find {x => resilience(x) < 15499.0 / 94744})
  //  892371480
  //  diffs: List(1, 2, 2, 6, 6, 6, 6, 30, 30, 30, 30, 30, 30, 210, 210, 210, 210, 210, 210, 210, 210, 210, 210, 2310, 2310, 2310, 2310, 2310, 2310, 2310, 2310, 2310, 2310, 2310, 2310, 30030, 30030, 30030, 30030, 30030, 30030, 30030, 30030, 30030, 30030, 30030, 30030, 30030, 30030, 30030, 30030, 510510, 510510, 510510, 510510, 510510, 510510, 510510, 510510, ...)
}
