package euler.tests

import euler.utils.{DiophantineTools, EulerTestsBase, EulerUtils, Fraction}
import euler.utils.FractionImplicits.`Int --> Fraction`

/**
  * Created on 23/07/2016.
  */
object ContinuousFractionE extends EulerTestsBase {
  val eSeq = EulerUtils.naturals flatMap { n => Seq(1, 2 * n, 1) }

  eulerAssertion((2.asFraction + DiophantineTools.valueOfPartialFractionSequence(eSeq take 3)).numer1, 11)
  eulerAssertion((2.asFraction + DiophantineTools.valueOfPartialFractionSequence(eSeq take 9)).numer1, 1457)

  val v100th = 2.asFraction + DiophantineTools.valueOfPartialFractionSequence(eSeq take 99)
  println(EulerUtils.splitToDigits(v100th.numer1).sum)
}
