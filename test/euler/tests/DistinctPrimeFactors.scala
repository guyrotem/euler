package euler.tests

import euler.utils.EulerUtils

/**
  * Created on 23/07/2016.
  */
object DistinctPrimeFactors extends App {

  def digConsecutive(num: Int, length: Int = 2): Boolean = {
    val sets = (0 until length) map {x => EulerUtils.fullDecompose(num + x)} map {_.toSet}
    sets.forall(_.size == length)
  }

  assert(!digConsecutive(13, 2))
  assert(digConsecutive(14, 2))

  assert(digConsecutive(644, 3))
  assert(!digConsecutive(241, 3))
  assert(!digConsecutive(20, 3))

  println(EulerUtils.naturals find {x => digConsecutive(x, 2)})
  println(EulerUtils.naturals find {x => digConsecutive(x, 3)})
  println(EulerUtils.naturals find {x => digConsecutive(x, 4)})
}
