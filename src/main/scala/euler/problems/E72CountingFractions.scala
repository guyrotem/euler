package euler.problems

import euler.utils.EulerMath

/**
  * Created on 09/09/2016.
  */
object E72CountingFractions extends App {
  def countProperFractionsUpTo(d: Int): BigInt = {
    (BigInt(2) to d) map EulerMath.totient sum
  }

  assert(countProperFractionsUpTo(8) == 21)

  println(countProperFractionsUpTo(1000000))
  //303963552391
}
