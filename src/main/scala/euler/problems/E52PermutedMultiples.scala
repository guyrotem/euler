package euler.problems

import euler.utils.EulerUtils

object E52PermutedMultiples extends App {

  def digitsOf(n: Int): List[Int] = {
    if (n == 0) Nil
    else List(n % 10) ++ digitsOf((n - (n % 10)) / 10)
  }

  def hasSameDigitsMultiplied(n: Int) = {
    val digs = digitsOf(n).sorted
    (1 to 5) forall { i =>
      digs == digitsOf((i + 1) * n).sorted
    }
  }

  println(
    EulerUtils.naturalsStartingAt(100001)
      .take(1000000 / 6 - 100000) //  Upper bound for 6 digits
      .find(hasSameDigitsMultiplied)
  ) //  Some(142857)
}
