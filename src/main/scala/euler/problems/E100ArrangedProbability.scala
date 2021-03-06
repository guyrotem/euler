package euler.problems

/**
  * Created on 16/07/2016.
  */
object E100ArrangedProbability extends App {
  def isFiftyChance(blueDiscs: BigInt, redDiscs: BigInt): Boolean = {
    calcChance(blueDiscs, redDiscs) match {
      case (n, d) => 2 * n == d
    }
  }

  def calcChance(blueDiscs: BigInt, redDiscs: BigInt): (BigInt, BigInt) = {
    //  b / (b + r) * (b - 1) / (b + r - 1)
    (
      blueDiscs * (blueDiscs - 1),
      (blueDiscs + redDiscs) * (blueDiscs + redDiscs - 1)
      )
  }

  def findBlueDiscsPerSum(sum: BigInt): BigInt = {
    def findTheLion(blue: BigInt, leapSize: BigInt): BigInt = {
      if (leapSize == 0) blue
      else calcChance(blue, sum - blue) match {
        case (n, d) =>
          if (2 * n == d) blue
          else if (2 * n < d) findTheLion(blue + leapSize, leapSize / 2)
          else findTheLion(blue - leapSize, leapSize / 2)
      }
    }

    findTheLion(7 * sum / 10 + 1, BigInt(1).<<(sum.bitLength - 2))
  }

  assert(isFiftyChance(3, 1))
  assert(isFiftyChance(15, 6))
  assert(isFiftyChance(85, 35))
  assert(isFiftyChance(493, 697 - 493))
  assert(isFiftyChance(2871, 4060 - 2871))
  assert(isFiftyChance(16731, 23661 - 16731))
  assert(isFiftyChance(97513, 137904 - 97513))

  assert(isFiftyChance(568345, 803761 - 568345))
  assert(isFiftyChance(3312555, 4684660 - 3312555))
  assert(!isFiftyChance(4, 3))
  assert(!isFiftyChance(123, 78))

  assert(findBlueDiscsPerSum(21) == 15)
  assert(findBlueDiscsPerSum(120) == 85)

//  val bigIntStream: Stream[BigInt] = BigInt("1070379100497") #:: (bigIntStream map {_ + 1})
  val bigIntStream: Stream[BigInt] = BigInt("2") #:: (bigIntStream map {_ + 1})
  val big50 = bigIntStream filter { discCount =>
    val bd = findBlueDiscsPerSum(discCount)
    val chance = calcChance(bd, discCount - bd)
    chance._2 == chance._1 * 2
  }
  val first10 = big50 take 1
  first10 foreach {x => println(findBlueDiscsPerSum(x) + " " + x)}

//756872327473 1070379110497

  // NOTE: https://www.alpertron.com.ar/QUAD.HTM
  val solStream: Stream[(BigInt, BigInt)] = (BigInt(4), BigInt(3)) #:: (solStream map {
    case (x, y) => (
      3 * x + 4 * y - 3,
      2 * x + 3 * y - 2
      )
  })
  println(solStream find {_._1 > BigInt("1000000000000")} map {_._2})
}
