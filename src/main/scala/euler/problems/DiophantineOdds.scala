package euler.problems

import euler.utils.DiophantineTools

/**
  * Created on 23/07/2016.
  */
object DiophantineOdds {
  def getPeriodLength(n: Int) = {
    DiophantineTools.rootAsContinuedFraction(n)._2.length
  }

  assume(getPeriodLength(2) == 1)
  assume(getPeriodLength(4) == 0)
  assume(getPeriodLength(23) == 4)

  val oddsCount = ((2 to 10000) map getPeriodLength) count {_ % 2 == 1}
  println(oddsCount)
}
