package euler.tests

import euler.utils.EulerUtils

/**
  * Created on 18/07/2016.
  */
object Totient extends App {

  def Totient(n: Int): Int = {
    val primeDivisors = EulerUtils.fullDecompose(n).keys
    val sum = n - primeDivisors.map({n/_}).sum
    sum
  }
}
