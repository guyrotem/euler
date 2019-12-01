package euler.problems

import euler.utils.EulerMath
import euler.utils.EulerMath._

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer
import scala.math.Ordering

object E500 extends App {

  val mod = 500500507

  def minNumberWith2PowDivisors(n: Int): Int = {
    val primes = getNPrimes(n)  //  multiply it to get largest number with 2^n divisors
    println(s"Calculated primes for n=$n")
    var headPointer = 0
    var tailPointer = n - 1
    var worthTheTrade = true
    val ds = new E500PopMinDs

    while (worthTheTrade && headPointer < tailPointer) {
      val bestLastFactorFromDs = ds.popMin(_.nextIncrement)
      val bestFactorFromDs = bestLastFactorFromDs.map(x => x.copy(exp = x.exp * 2 + 1))
      val bestFactorFromQueue = Factor(primes(headPointer), 3)
      val baseGain = primes(tailPointer)

      val bestReplacement = (bestFactorFromDs.toList :+ bestFactorFromQueue).minBy(_.lastIncrement)
      val useQueueValue = bestReplacement ==  bestFactorFromQueue

      worthTheTrade = baseGain > bestReplacement.lastIncrement

      if (useQueueValue || !worthTheTrade) {
        bestLastFactorFromDs.foreach(ds.add)
      }

      if (useQueueValue && worthTheTrade) {
        headPointer = headPointer + 1
      }

      if (worthTheTrade) {
        ds.add(bestReplacement)
        tailPointer = tailPointer - 1
      }

    }

    (headPointer to tailPointer) foreach(primeExp1 => ds.add(Factor(primes(primeExp1), 1)))

    //  debug
    if (n < 100) println(ds.getAll)

    calcProductMod(ds, mod)

  }

  private def calcProductMod(ds: PopMinDs[Factor], mod: Int) = {
    ds.getAll.map({
      case Factor(base, exp) => powMod(base, exp, mod)
    }).productMod(mod)
  }

  implicit class IterableInts(iterableInt: IterableOnce[Int]) {
    def productMod(mod: Int): Int = iterableInt.iterator.fold(1) {
      case (acc, next) => multiplyMod(acc, next, mod)
    }
  }

  def getNPrimes(n: Int): ArraySeq[Int] = {
    //  force the stream into an array to allow efficient access (Stream <: Seq)
    scala.collection.immutable.ArraySeq.from(
      //  this is the slowest part
      EulerMath.makePrimeStream.take(n)
    )
  }

  def findExchange(factorized: ArrayBuffer[(Int, Int)], index: Int) = {
    val minIndex = (0 until index).minBy(x => powInt(factorized(x)._1, factorized(x)._2 + 1))
    Some(factorized(minIndex))
      .filter(min =>
        factorized(index)._1 > powInt(min._1, min._2 + 1)
      )
  }

  (1 to 12).map(minNumberWith2PowDivisors).map(println)
  //  2 6 24 120 840 7560 83160
  assert(minNumberWith2PowDivisors(4) == 120)
  assert(minNumberWith2PowDivisors(8) == 1081080)
  //  println(minNumberWith2PowDivisors(500500)) // 35407281

}

//  utils
trait PopMinDs[T] {
  def popMin[B](f: T => B)(implicit cmp: Ordering[B]): Option[T]
  def add(t: T): Unit

  def getAll: List[T]
}

case class Factor(base: Int, exp: Int) {
  val nextIncrement: Double = Math.pow(base, exp + 1)
  val lastIncrement: Double = Math.pow(base, exp/2 + 1)
}

class E500PopMinDs extends PopMinDs[Factor] {

  private val ds = scala.collection.mutable.SortedMap[Double, Factor]()

  override def popMin[B](quantifier: Factor => B)(implicit cmp: Ordering[B]): Option[Factor] = {
    ds.headOption.map({
      case (k, v) => ds.remove(k); v
    })
  }

  override def add(factor: Factor): Unit = ds.addOne(factor.nextIncrement -> factor)

  override def getAll: List[Factor] = ds.toList.map(_._2)
}
