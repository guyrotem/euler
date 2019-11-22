package euler.problems

/**
  * Created on 30/07/2016.
  */
object E145ReversibleInts extends App {
  private def reverseInt(n: Int): Int = {
    n.toString.reverse.toInt
  }

  private def isReversible(n: Int): Boolean = {
    (n.toString.last != "0".charAt(0)) && ((n + reverseInt(n)).toString forall {x => "13579".contains(x)})
  }

  val revCount1000 = (1 to 1000) count isReversible
  assert(revCount1000 == 120)

  println(1 to 100000 filter isReversible)

  val revCount100000000 = (1 to 100000000) count isReversible
  println(revCount100000000)

  //608720
  //  brute force works... appareantly there are no solutions for length: 4k-3 (1,5,9,...)
}
