package euler.problems

/**
  * Created on 23/07/2016.
  */
object Lycherals extends App {
  def isLycheral(num: Int): Boolean = {
    lazy val lychStream: Stream[BigInt] = BigInt(num) #:: (lychStream map lychIteration)
    lychStream.tail.take(50) forall isNotPalindrome
  }

  private def isNotPalindrome(num: BigInt): Boolean = {
    num.toString.reverse != num.toString
  }

  private def lychIteration(num: BigInt): BigInt = {
    val reverse = BigInt(num.toString().reverse)
    num + reverse
  }

  assert(!isLycheral(47))
  assert(!isLycheral(349))
  assert(isLycheral(4994))

  println((1 to 9999) count isLycheral)
}
