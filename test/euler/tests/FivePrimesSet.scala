package euler.tests

import euler.utils.EulerUtils

/**
  * Created on 15/07/2016.
  */
object FivePrimesSet extends App {

  def makePrimeStream(numbersStream: Stream[Int]) = {
    numbersStream filter EulerUtils.sqrtPrimeCheck
  }

  val naturals: Stream[Int] = 1 #:: (naturals map {_ + 1})

  val primeStream = makePrimeStream(naturals.tail)

//  println(primeStream take(5) toList)

  def isPrimeConcat(primes: (Int, Int)): Boolean = {
    try {
      primes match {
        case (a, b) =>
          EulerUtils.sqrtPrimeCheck((a.toString + b.toString).toInt) &&
            EulerUtils.sqrtPrimeCheck((b.toString + a.toString).toInt)
      }
    } catch {
      case e: Exception => false
    }
  }

  def isMultiPrime(primes: Seq[Int]): Boolean = {
    val allGroups = for {
      i <- primes.indices
      j <- (i + 1) until primes.length
    } yield (primes(i), primes(j))
    allGroups.forall(isPrimeConcat)
  }

  assert(!isMultiPrime(Seq(2, 5)))
  assert(isMultiPrime(Seq(3, 7, 109, 673)))

  def generateGroupsOf2(stream: Stream[Int], limit: Int): Seq[Seq[Int]] = {
    val boundedStream = (stream take limit).toList
    for {
      i <- 0 until limit
      j <- (i+1) until limit
    } yield Seq(boundedStream(i), boundedStream(j))
  }

  def generateGroupsOf5(stream: Stream[Int], limit: Int): Seq[Seq[Int]] = {
    val boundedStream = (stream take limit).toList
    for {
      i <- 0 until limit
      j <- (i+1) until limit
      k <- (j+1) until limit
      l <- (k+1) until limit
      m <- (l+1) until limit
    } yield Seq(boundedStream(i), boundedStream(j), boundedStream(k), boundedStream(l), boundedStream(m))
  }

//  println(generateGroupsOf5(primeStream, 7))
  println("start!")

  def extendAllGroups(base: MultiPrimeSet): MultiPrimeSet = {
    val nextSeq = base.primesSet flatMap { x: Set[Int] =>
      val p: Seq[Int] = base.source filter {y => x forall {z => y > z && isPrimeConcat((z, y))} }
      p flatMap {z: Int => Set[Set[Int]](x + z)}
    }
    MultiPrimeSet(nextSeq, base.level + 1, base.source)
  }

//  find all prime sets made out of the 1st 3000 primes, containing at least 1 form the 1st 10
  val base1 = MultiPrimeSet(primeStream take 10 map {x => Set(x)} toSet, 1, primeStream take 3000 toList)
  println(base1.primesSet.size)
  val base2 = extendAllGroups(base1)
  println(base2.primesSet.size)
  val base3 = extendAllGroups(base2)
  println(base3.primesSet.size)
  val base4 = extendAllGroups(base3)
  println(base4.primesSet.size)
  val base5 = extendAllGroups(base4)
  println(base5.primesSet.size)

  println(base5.primesSet)
  //Set(Set(2341, 1237, 18433, 7, 12409), Set(5701, 8389, 13, 5197, 6733))
}
