package euler.tests

import euler.utils.EulerMath.BigFraction
import euler.utils.{EulerMath, EulerTestsBase, EulerUtils}

/**
  * Created on 06/08/2016.
  */
object E329PrimeFrog extends EulerTestsBase {

  def solveForIndex(calls: Seq[Boolean], frogSpaceSize: Int)(num: Int): BigFraction = {
    assert(0 < num && num <= frogSpaceSize, num)

    val sequenceLength = calls.length

    val frogSpace = sequenceLength.to(2, -1) ++ (1 to frogSpaceSize) ++ (frogSpaceSize - 1).to(frogSpaceSize - sequenceLength, -1)
    val zeroIndex = sequenceLength - 2

    val isPrime = (0 to frogSpaceSize) map EulerUtils.sqrtPrimeCheck
    val startAt = zeroIndex + num

    val one = EulerMath.CreateFraction(1, 1)
    val sixth = EulerMath.CreateFraction(1, 6)
    val third = EulerMath.CreateFraction(1, 3)

    def help(beginAt: Int, seqLeft: Seq[Boolean]): BigFraction = {
      seqLeft.headOption map { isPrimeSeq =>
        val isPrimeStart = isPrime(frogSpace(beginAt))
        val pOk = if (isPrimeSeq ^ isPrimeStart) sixth else third

        pOk * (help(beginAt + 1, seqLeft.tail) + help(beginAt - 1, seqLeft.tail))
      } getOrElse one
    }

    help(startAt, calls)
  }

  val expectedSeq = "PPPPNNPPPNPPNPN" map {ch => if (ch == "P".charAt(0)) true else false}
  val frogSpaceSize = 500
  val solutionPerStart = (1 to frogSpaceSize) map solveForIndex(expectedSeq, frogSpaceSize)
  val averageResult = solutionPerStart.foldLeft(EulerMath.CreateFraction(0, 1))({_ + _}) / EulerMath.CreateFraction(500, 1)

  println(averageResult)
  // 199740353 / 29386561536000

}
