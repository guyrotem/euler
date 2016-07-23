package euler.tests

import scala.collection.immutable.IndexedSeq

/**
  * Created on 23/07/2016.
  */
object aaaPanDigitalSubStringDivisible extends App {
  def is09PanDigital(num: BigInt): Boolean = {
    num.toString.length == 10 && ((0 to 9) forall {x => num.toString().contains(x.toString)})
  }

  assert(is09PanDigital(BigInt("2091873645")))
  assert(!is09PanDigital(BigInt("291873645")))
  assert(!is09PanDigital(BigInt("29018731645")))

  val divisibleBy2 = (1 to 999/2) map {_ * 2}
  val divisibleBy3 = (1 to 999/3) map {_ * 3}
  val divisibleBy5 = (1 to 999/5) map {_ * 5}
  val divisibleBy7 = (1 to 999/7) map {_ * 7}
  val divisibleBy11 = (1 to 999/11) map {_ * 11}
  val divisibleBy13 = (1 to 999/13) map {_ * 13}
  val divisibleBy17 = (1 to 999/17) map {_ * 17}

  def neighborsRight(right: Seq[Int]): (Int) => Boolean = {
    val rightP = right map {_.toString.take(2)}
    (left: Int) => {rightP contains left.toString.takeRight(2)}
  }

  def neighborsLeft(left: Seq[Int]): (Int) => Boolean = {
    val leftP = left map {_.toString.takeRight(2)}
    (right: Int) => {leftP contains right.toString.take(2)}
  }

  def idgafDivisibilityProperty(num: BigInt): Boolean = {
    val numStr = num.toString()
    numStr.substring(3, 4).toInt % 2 == 0 &&
    numStr.substring(2, 5).toInt % 3 == 0 &&
    numStr.substring(5, 6).toInt % 5 == 0 &&
    numStr.substring(4, 7).toInt % 7 == 0 &&
    numStr.substring(5, 8).toInt % 11 == 0 &&
    numStr.substring(6, 9).toInt % 13 == 0 &&
    numStr.substring(7).toInt % 17 == 0
  }
  assert(idgafDivisibilityProperty(BigInt(1406357289)))

  def createBigStream(start: BigInt, stop: BigInt): Stream[BigInt] = {
    if (start > stop) Stream.empty
    else start #:: createBigStream(start + 1, stop)
  }

  def buildAvailableShit() = {
    val possible2 = divisibleBy2 filter neighborsRight(divisibleBy3)
    val possible3 = divisibleBy3 filter neighborsRight(divisibleBy5) filter neighborsLeft(divisibleBy2)
    val possible5 = divisibleBy5 filter neighborsRight(divisibleBy7) filter neighborsLeft(divisibleBy3)
    val possible7 = divisibleBy7 filter neighborsRight(divisibleBy11) filter neighborsLeft(divisibleBy5)
    val possible11 = divisibleBy11 filter neighborsRight(divisibleBy13) filter neighborsLeft(divisibleBy7)
    val possible13 = divisibleBy13 filter neighborsRight(divisibleBy17) filter neighborsLeft(divisibleBy11)
    val possible17 = divisibleBy17 filter neighborsLeft(divisibleBy13)

    Seq(possible2, possible3, possible5, possible7, possible11, possible13, possible17, 0 to 999, 0 to 999)
  }

  val processed: Seq[Seq[Int]] = buildAvailableShit()

  val leftDigit = (1 to 9) map {_.toString}

//  leftDigit flatMap { ld =>
//    val constraints: Seq[(Seq[Int], (Seq[Int], Seq[Int]))] = processed.zip(processed.tail zip processed.tail.tail)
//  }

//  val panShitInts = createBigStream(BigInt(1234567890), BigInt("9876543210")) filter is09PanDigital filter idgafDivisibilityProperty

//  println(panShitInts.length)
//  println(panShitInts.sum)
}
