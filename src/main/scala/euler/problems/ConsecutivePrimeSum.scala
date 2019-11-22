package euler.problems

import euler.utils.EulerUtils

/**
  * Created on 24/07/2016.
  */
object ConsecutivePrimeSum extends App {
  val primes = EulerUtils.naturals filter EulerUtils.sqrtPrimeCheck
  val primesList = primes.take(primes indexWhere {_ >= 999999}).toList
//  lazy val primeSum: Stream[Int] = sumStream(primes)

//  println(primeSum.take(100).toList)
//  println(primeSum(21))
  println(primes.take(200).toList)

  def splitToConsecutivePrimes(num: Int): (Int, Int) = {
    val primeIndex = primesList indexWhere {_ >= (num / 100)} //300
    val trySum = (0 to primeIndex) find { startAt =>
      val thatSum = sumList(primesList.drop(startAt))
      val item = thatSum find {_ >= num}
      item.get == num
    }
    (trySum map { index =>
      (num, (sumList(primesList.drop(index)) indexOf num) + 1)
    }).getOrElse(num, 0)
  }

  def sumStream(stream: Stream[Int]): Stream[Int] = {
    lazy val result: Stream[Int] = stream.head #:: ((result zip stream.tail) map {x => x._1 + x._2})
    result
  }

  def sumList(stream: List[Int]): Stream[Int] = {
    lazy val result: Stream[Int] = stream.head #:: ((result zip stream.tail) map {x => x._1 + x._2})
    result
  }

  assert(splitToConsecutivePrimes(41)._2 == 6)
  assert(splitToConsecutivePrimes(953)._2 == 21)

  val res = (950000 to 1000000) filter EulerUtils.sqrtPrimeCheck map splitToConsecutivePrimes
  val max = (res map {_._2}).max
  val number = (res find {_._2 == max}).get._1
  println(s"max is $max, res == $number")
}
