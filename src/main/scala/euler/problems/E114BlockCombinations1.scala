package euler.problems

import euler.utils.Memoizer

object E114BlockCombinations1 extends App {

  class RedBlockCombinator(m: Int) {

    private val memoizer = new Memoizer[Int, BigInt](countAll)

    def countAll(totalLength: Int): BigInt = {
      if (totalLength < m) 1
      else if (totalLength == m) 2
      else
        (m to totalLength).map { usage =>
          memoizer.get(totalLength - usage - 1)
        }.sum + memoizer.get(totalLength - 1)
    }
  }

  //  E114
  assert(new RedBlockCombinator(3).countAll(5) == 7)
  assert(new RedBlockCombinator(3).countAll(7) == 17)
  println(new RedBlockCombinator(3).countAll(50)) //  16475640049

  //  E115
  val redBlockCombinator50 = new RedBlockCombinator(50)
  (1 to 500) find {x => redBlockCombinator50.countAll(x) > 1000000} //  Some(168)

}
