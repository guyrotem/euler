package euler.problems

import euler.utils.EulerUtils

/**
  * Created on 24/07/2016.
  */
object DiophantineReciprocals1 extends App {
  //  x^-1 + y^-1 = N^-1 <=> Nx + Ny = xy <=> xy - Nx - Ny = 0
  //  ... => (x-N)(y-N) - N^2 = 0
  //  decompose N^2:
  //  a) if N is prime, the only solution is the trivial solution, x=y=2N
  //  b) set A'=x-N, B'=y-N, find all solutions in terms of [A',B']: A'B'=N^2, then add N (IDC)

  def getNumberOfSolutions(num: BigInt): Int = {
    //NOTE: it is assumed that "num" is square!
    val primes = EulerUtils.fullDecomposeBig(num * num)
    //  every possible division is duplicated (x,y) <=> (y,x)
    //  except for (sqrt(N), sqrt(N)), as num is square!
    ((primes map {_._2 + 1}).product + 1)/2
  }

  assert(getNumberOfSolutions(1) == 1)
  assert(getNumberOfSolutions(2) == 2)
  assert(getNumberOfSolutions(3) == 2)
  assert(getNumberOfSolutions(4) == 3)
  assert(getNumberOfSolutions(5) == 2)
  assert(getNumberOfSolutions(6) == 5)

  val resultsSequence = {
    def createRepeatedSeq[T](diffsAndCount: (T, Int)): Seq[T] = {
      List.fill(diffsAndCount._2)(diffsAndCount._1)
    }

    val primes: Stream[Int] = EulerUtils.naturals filter EulerUtils.sqrtPrimeCheck
    val numDuplicates: Stream[Int] = primes map {_ - 1}
    lazy val diffs: Stream[BigInt] = 1 #:: (diffs zip primes map {tuple => tuple._1 * tuple._2})
    val actualDiffs: Stream[BigInt] = (diffs zip numDuplicates) flatMap createRepeatedSeq
    lazy val actualSeries: Stream[BigInt] = 1 #:: (actualSeries zip actualDiffs map {tuple => tuple._1 + tuple._2})

//    println(numDuplicates.take(10).toList)
//    println(diffs.take(10).toList)
//    println(actualDiffs.take(15).toList)
    actualSeries
  }

  def atLeastNSolutions(solutionsCount: Int): (BigInt, Int) = {
    ((resultsSequence zip (resultsSequence map getNumberOfSolutions)) find {_._2 > solutionsCount}).get
  }

  assert(atLeastNSolutions(100) == (1260, 113))
  assert(atLeastNSolutions(1000) == (180180, 1013), "Wrong! " + atLeastNSolutions(1000))
//  println(resultsSequence.slice(240, 280).toList)
  println(resultsSequence.take(50).toList)

//  val solution = (resultsSequence zip (resultsSequence map getNumberOfSolutions)) find {_._2 > 4000000}
  val solution = atLeastNSolutions(4000000)
  println(solution)
  println(getNumberOfSolutions(BigInt("10953009486979560")))

  println((BigInt(1) to 1000) map getNumberOfSolutions)

  def atLeastNSolutions2(solutionsCount: Int, start: BigInt): (BigInt, Int) = {
    val vector: Stream[BigInt] = bigNaturalsStartingAt(start)
    ((vector zip (vector map getNumberOfSolutions)) find {_._2 >= solutionsCount}).get
  }

  def bigNaturalsStartingAt(num: BigInt): Stream[BigInt] = {
    num #:: bigNaturalsStartingAt(num + 1)
  }
  var n = 50
  var vv: (BigInt, Int) = (BigInt(1), 1)
  while (n > 0) {
    println(s"$vv")
    vv = atLeastNSolutions2(vv._2, vv._1 + 1)
    n = n - 1
  }
//  MAX seq
//  (2,2)
//  (3,2)
//  (4,3)
//  (6,5)
//  (10,5)
//  (12,8)
//  (18,8)
//  (20,8)
//  (24,11)
//  (30,14)
//  (42,14)
//  (48,14)
//  (60,23)
//  (84,23)
//  (90,23)
//  (120,32)
//  (168,32)
//  (180,38)
//  (210,41)
//  (240,41)
//  (330,41)
//  (336,41)
//  (360,53)
//  (420,68)
//  (630,68)
//  (660,68)
//  (720,68)
//  (780,68)
//  (840,95)
//  (1260,113)
//  (1680,122)
//  (2310,122)
//  (2520,158)
//  (3780,158)
//  (3960,158)
//  (4200,158)
//  (4620,203)
//  (5040,203)
//  (5460,203)
//  (6930,203)
//  (7140,203)
//  (7560,221)
//  (9240,284)
//  (10920,284)
//  (13860,338)
//  (16380,338)
//  (18480,365)
//  (21840,365)
//  (27720,473)
//  (32760,473)
}
