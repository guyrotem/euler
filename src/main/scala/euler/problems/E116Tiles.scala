package euler.problems

import euler.utils.Memoizer

object E116Tiles extends App {

  class ReplacementsCounter(replacementLength: Int) {

    private val memoizationBox = new Memoizer[(Int, Int), BigInt]((x: (Int, Int)) => countReplacements(x._1, x._2))

    def countReplacements(totalLength: Int, numberOfTiles: Int): BigInt = {
      assert(numberOfTiles > 0)
      if (totalLength < replacementLength)
        0
      else if (numberOfTiles == 1)
        totalLength - replacementLength + 1
      else
        calcMemoized(totalLength - replacementLength, numberOfTiles - 1) +
          calcMemoized(totalLength - 1, numberOfTiles)
    }

    private def calcMemoized(totalLength: Int, numberOfTiles: Int): BigInt = {
      memoizationBox.get(totalLength -> numberOfTiles)
    }
  }

  def countAll(totalLength: Int) = (2 to 4).map({ replacementLength =>
    val replacementsCounter = new ReplacementsCounter(replacementLength)
    (1 to (totalLength / replacementLength)).map({ numberOfBlocks =>
      replacementsCounter.countReplacements(totalLength, numberOfBlocks)
    }).sum
  }).sum

  assert(countAll(5) == 12)
//  assert(countAll(50) == BigInt("20492570929"))

}
