package euler.utils

import FractionImplicits.`Int --> Fraction`

/**
  * Created on 22/07/2016.
  */
object DiophantineTests extends EulerTestsBase {
  eulerAssertion(DiophantineTools.rootAsContinuedFraction(2), (1, Seq(2)))
  eulerAssertion(DiophantineTools.rootAsContinuedFraction(3), (1, Seq(1, 2)))
  eulerAssertion(DiophantineTools.rootAsContinuedFraction(4), (2, Seq.empty))
  eulerAssertion(DiophantineTools.rootAsContinuedFraction(5), (2, Seq(4)))
  eulerAssertion(DiophantineTools.rootAsContinuedFraction(6), (2, Seq(2, 4)))
  eulerAssertion(DiophantineTools.rootAsContinuedFraction(7), (2, Seq(1, 1, 1, 4)))
  eulerAssertion(DiophantineTools.rootAsContinuedFraction(23), (4, Seq(1, 3, 1, 8)))

  eulerAssertion(DiophantineTools.valueOfPartialFractionSequence(Seq(1)), 1.asFraction)
  eulerAssertion(DiophantineTools.valueOfPartialFractionSequence(Seq(1, 2)), new Fraction(2, 3))

  eulerAssertion(new Fraction(8, 10).numer1, 4)
  eulerAssertion(new Fraction(8, 10).denum1, 5)

  val sqrt2approx: (Int, Seq[Int]) = DiophantineTools.rootAsContinuedFraction(2)

  eulerAssertion(
    sqrt2approx._1.asFraction + DiophantineTools.calculateDiophantineSequenceValue(sqrt2approx._2, 3),
    new Fraction(17, 12)
  )
}
