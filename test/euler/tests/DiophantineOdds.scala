package euler.tests

import euler.utils.{DiophantineTools, EulerTestsBase}

/**
  * Created on 23/07/2016.
  */
object DiophantineOdds extends EulerTestsBase {
  def getPeriodLength(n: Int) = {
    DiophantineTools.rootAsContinuedFraction(n)._2.length
  }

  eulerAssertion(getPeriodLength(2), 1)
  eulerAssertion(getPeriodLength(4), 0)
  eulerAssertion(getPeriodLength(23), 4)

  val oddsCount = ((2 to 10000) map getPeriodLength) count {_ % 2 == 1}
  println(oddsCount)
}
