package euler.tests

import euler.utils.Utils

/**
  * Created on 16/07/2016.
  */
object SumFactorial extends App {
  def factorial(n: Int): Int = {
    assert(n > -1)
    if (n == 0) 1
    else n * factorial(n - 1)
  }

  val ok = (3 to 2540160) filter {x =>
    (Utils.splitToDigits(x) map factorial).sum == x
  }

  println(ok)
  println(ok.sum)
}
