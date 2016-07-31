package euler.tests

import euler.utils.EulerUtils

/**
  * Created on 25/07/2016.
  */
object E112BouncyNumbers extends App {
  def isIncreasing(num: Int): Boolean = {
    val digits = EulerUtils.splitToDigits(num)
    (digits zip digits.tail) forall {x => x._1 <= x._2}
  }

  def isDecreasing(num: Int): Boolean = {
    val digits = EulerUtils.splitToDigits(num)
    (digits zip digits.tail) forall {x => x._1 >= x._2}
  }

  def isBouncy(num: Int) = !isIncreasing(num) && !isDecreasing(num)

  assert(isBouncy(153))
  assert(!isBouncy(1134889))
  assert(!isBouncy(866500))

  val isBouncyStream = EulerUtils.naturals map isBouncy

  lazy val sumStream: Stream[(Int, Int)] = (1, 0) #:: ((sumStream zip isBouncyStream.tail) map {x => if(x._2) (x._1._1, x._1._2 + 1) else (x._1._1 + 1, x._1._2) })

  println(sumStream find {x => x._2 >= 99 * x._1} map {x => x._1 + x._2})
}
