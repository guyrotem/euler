package euler.problems

import scala.language.postfixOps

object E265BinaryCircles extends App {
  def solve(n: Int): Seq[Seq[Int]] = {
    assume(n < 31)
    val circleSize = Math.pow(2, n).toInt

    def getNextNodes(current: Int): Seq[Int] =
      Seq((current * 2) % circleSize, (current * 2 + 1) % circleSize)

    def traverse(path: Seq[Int]): Seq[Seq[Int]] = {
      if (path.length == circleSize)
        Seq(path)
      else
        getNextNodes(path.head) filterNot path.contains map {_ +: path} flatMap traverse
    }

    traverse(Seq(0)) map {_.reverse}
  }

  def formatAsCyclic(path: Seq[Int]): Seq[Int] = {
    path map {_ / (path.length / 2)}
  }

  def asNumber(binaryPath: Seq[Int]): Int = {
    binaryPath.foldLeft(0) {(acc, current) => 2 * acc + current}
  }

  assert(solve(2) == Seq(Seq(0, 1, 3, 2)))
  assert(solve(3) == Seq(
    Seq(0, 1, 2, 5, 3, 7, 6, 4),
    Seq(0, 1, 3, 7, 6, 5, 2, 4)
  ))
  assert((solve(3) map formatAsCyclic map asNumber) == Seq(23, 29))

  println(solve(5) map formatAsCyclic map asNumber map {BigInt(_)} sum)
  //209110240768
}
