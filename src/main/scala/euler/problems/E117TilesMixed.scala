package euler.problems

import euler.utils.Memoizer

object E117TilesMixed extends App {

  class TilesPlacementCounter(tileLengths: List[Int], memoize: Boolean = true) {

    assert(tileLengths.nonEmpty)

    private val memoizationBox = new Memoizer[(Int, Int), BigInt]((x: (Int, Int)) => countReplacements(x._1, x._2))
    private val minTileLength = tileLengths.min

    def countReplacements(totalLength: Int, numberOfTiles: Int): BigInt = {
      assert(numberOfTiles > 0)
      if (totalLength < minTileLength * numberOfTiles)
        0
      else if (numberOfTiles == 1)
        tileLengths.map(l =>
          Math.max(0, totalLength - l + 1)
        ).sum
      else
          tileLengths.map { tileLength =>
            calcMemoized(totalLength - tileLength, numberOfTiles - 1)
          }.sum +
            calcMemoized(totalLength - 1, numberOfTiles)
    }

    private def calcMemoized(totalLength: Int, numberOfTiles: Int): BigInt = {
      if (memoize)
        memoizationBox.get(totalLength -> numberOfTiles)
      else
        countReplacements(totalLength, numberOfTiles)
    }
  }

  val tileLengths = List(2, 3, 4)

  def countAll(totalLength: Int) = {
    val replacementsCounter = new TilesPlacementCounter(tileLengths, true)
    (1 to (totalLength / tileLengths.min)).map { numberOfTiles =>
      replacementsCounter.countReplacements(totalLength, numberOfTiles)
    }.sum + 1
  }

  assert(countAll(5) == 15)
  //  assert(countAll(50) == BigInt("20492570929"))
  //  1 1 1 4 f(n-1)+f(n-2)+f(n-3)+f(n-4)

}
