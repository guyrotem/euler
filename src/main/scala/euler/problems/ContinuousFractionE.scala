package euler.problems

import euler.utils.{DiophantineTools, EulerUtils}
import euler.utils.FractionImplicits._

/**
  * Created on 23/07/2016.
  */
object ContinuousFractionE {
  val eSeq = EulerUtils.naturals flatMap { n => Seq(1, 2 * n, 1) }

  assume((2.asFraction + DiophantineTools.valueOfPartialFractionSequence(eSeq take 3)).numer1 == 11)
  assume((2.asFraction + DiophantineTools.valueOfPartialFractionSequence(eSeq take 9)).numer1 == 1457)

  val v100th = 2.asFraction + DiophantineTools.valueOfPartialFractionSequence(eSeq take 99)
  println(EulerUtils.splitToDigits(v100th.numer1).sum)
}
