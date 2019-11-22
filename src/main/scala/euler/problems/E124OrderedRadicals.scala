package euler.problems

import euler.utils.EulerUtils

/**
  * Created on 25/07/2016.
  */
object E124OrderedRadicals extends App {
  val vector = 1 to 100000
  val list = vector zip (vector map EulerUtils.fullDecompose map {_.keys.product})

  val out = list.sortBy({ _._2})

  println(out(9999))
}
