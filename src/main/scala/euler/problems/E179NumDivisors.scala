package euler.problems

import euler.utils.EulerUtils

/**
  * Created on 31/07/2016.
  */
object E179NumDivisors extends App {
  def numDivisors(num: Int): Int = {
    EulerUtils.fullDecompose(num).values map {_ + 1} product
  }

  val divisorsCount = (2 to 10000000) map numDivisors

  println(divisorsCount zip divisorsCount.tail count {x => x._1 == x._2})
}
