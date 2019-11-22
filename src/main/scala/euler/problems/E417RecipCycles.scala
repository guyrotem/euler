package euler.problems

/**
  * Created on 17/08/2016.
  */
object E417RecipCycles extends App {
  def calcCycle(num: Int): Int = {

    def help(history: Seq[Int]): Int = {
      val next = (history.head * 10) % num
      if (next == 0) 0
      else if (history contains next) history.indexOf(next) + 1
      else help(next +: history)
    }
    help(Seq(1))
  }

  assert(calcCycle(3) == 1)
  assert(calcCycle(4) == 0)
  assert(calcCycle(5) == 0)
  assert(calcCycle(6) == 1)
  assert(calcCycle(7) == 6)
  println(calcCycle(14 * 13))

  println((Seq(0, 0, 0) ++ ((3 to 500) map calcCycle)) zipWithIndex)
}
