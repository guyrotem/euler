package euler.tests

import euler.utils.EulerUtils

/**
  * Created on 23/07/2016.
  */
object SquareDigitChains extends App {
  def sumSquareDigits(num: Int) = {
    (EulerUtils.splitToDigits(num) map {x => x * x}).sum
  }

  def createChain(num: Int): Stream[Int] = {
    lazy val st: Stream[Int] = num #:: (st map sumSquareDigits)
    st
  }

  assert((createChain(44) indexOf 1) == 4)
  assert((createChain(85) indexOf 89) == 1)

  def is89(num: Int): Boolean = {
    val loop = createChain(num) find {x => x == 89 || x == 1}
    loop.get == 89
  }

  assert(is89(85))
  assert(!is89(44))

  println((1 to 9999999) count is89)

}
