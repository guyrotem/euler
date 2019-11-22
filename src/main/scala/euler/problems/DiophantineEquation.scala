package euler.problems

import euler.utils.{DiophantineTools, EulerUtils, Fraction}
import euler.utils.FractionImplicits._

/**
  * Created on 22/07/2016.
  */
object DiophantineEquation extends App {
  //  See Pell's equation and continued fractions for more info!
  //x^2 - Dy^2 = 1
  def findMinX(D: Int): BigInt = {
    val fractionResult = DiophantineTools.rootAsContinuedFraction(D) match {
      case (_, Seq()) => None
      case (n, seq) => EulerUtils.naturals map {
        n.asFraction + DiophantineTools.calculateDiophantineSequenceValue(seq, _)
      } find {
        fr: Fraction => checkXYD(fr.numer1, fr.denum1, D)
      }
    }
    fractionResult map {_.numer1} getOrElse(-1)
  }

  def checkXYD(x: BigInt, y: BigInt, D: Int): Boolean = {
    x * x - D * y * y == 1
  }

  assert(EulerUtils.bigIntSqRootCeil(9) == 3)
  assert(EulerUtils.bigIntSqRootCeil(BigInt("10000000000000000")) == BigInt("100000000"))
  assert(findMinX(5) == 9)
  assert(findMinX(6) == 5)
  assert(findMinX(7) == 8)
  assert(findMinX(13) == 649)

  val allX = (1 to 1000) map findMinX

  println(allX.max)
  println((allX indexOf allX.max) + 1)
//  16421658242965910275055840472270471049
//  661
}
