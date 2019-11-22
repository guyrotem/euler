package euler.problems

import euler.utils.EulerMath
import euler.utils.EulerMath.BigFraction

/**
  * Created on 25/08/2016.
  */
object E121DiscGamePrizeFund extends App {

  val zero = EulerMath.CreateFraction(0, 1)
  val one = EulerMath.CreateFraction(1, 1)

  def gameChance(turns: Int): BigFraction = {
    val dist = distribution(turns)
    val moreBlueDiscs = (turns / 2 + 1) to turns
    (moreBlueDiscs map {x => dist(x)}) reduceLeft {_ + _}
  }

  def winnerPrize(chance: BigFraction): Int = {
    //  (x - 1) * chance < (1 - chance)
    //  x * chance < 1
    //  x < 1 / chance
    //  x == floor(1/chance)
    (chance.d / chance.n).toInt
  }

  private def distribution(turns: Int): Map[Int, BigFraction] = {
    if (turns == 1) {
      Map(
        0 -> EulerMath.CreateFraction(1, 2),
        1 -> EulerMath.CreateFraction(1, 2)
      )
    } else if (turns > 1) {
      val blueInCurrentRound = EulerMath.CreateFraction(1, turns + 1)
      val lastDistribution = distribution(turns - 1)

      val pairs = (0 to turns) map {blueDiscs =>
        blueDiscs ->
          ((lastDistribution.getOrElse(blueDiscs, zero) * (one - blueInCurrentRound))
          + (lastDistribution.getOrElse(blueDiscs - 1, zero) * blueInCurrentRound))
      }

      pairs.toMap
    } else {
      throw new IllegalArgumentException()
    }
  }

  assert(gameChance(1) == EulerMath.CreateFraction(1, 2))
  assert(gameChance(2) == EulerMath.CreateFraction(1, 6))
  assert(gameChance(3) == EulerMath.CreateFraction(1, 24) + EulerMath.CreateFraction(1, 8) + EulerMath.CreateFraction(1, 12) + EulerMath.CreateFraction(1, 24))
  assert(gameChance(4) == EulerMath.CreateFraction(11, 120))

  assert(winnerPrize(EulerMath.CreateFraction(11, 120)) == 10)

  val chance = gameChance(15)
  println(s"chance of winning: $chance, prize: ${winnerPrize(chance)}")
  //  chance of winning: 9219406943 / 20922789888000, prize: 2269
}
