package euler.tests

import euler.utils.EulerUtils

/**
  * Created on 24/09/2016.
  */
object Test112233 extends App {
  def makeTripletStream[T](s: Stream[T]): Stream[(T, T, T)] = {
    if (s.isEmpty) Stream.empty
    else {
      (makeDoubleStream(s.tail) map {res => (s.head, res._1, res._2)}) #::: makeTripletStream(s.tail)
    }
  }

  def makeDoubleStream[T](s: Stream[T]): Stream[(T, T)] = {
    if (s.isEmpty) Stream.empty
    else {
      (s.tail map {res => (s.head, res)}) #::: makeDoubleStream(s.tail)
    }
  }

  println(makeTripletStream((1 to 100) toStream))

  case class Base420(pow2: Int, pow4: Int, pow6: Int) {
    def asNumber: BigInt = {
      2 *
        Seq.fill(2)(BigInt(pow2)).product *
        Seq.fill(4)(BigInt(pow4)).product *
        Seq.fill(6)(BigInt(pow6)).product
    }
  }

  def getAllNumberExtensions(number: BigInt, limit: BigInt, extensions: Seq[Int]): Seq[BigInt] = {
    def extendWith(current: BigInt, multipliers: Seq[Int]): Seq[BigInt] = {
      if (multipliers.isEmpty) List(current)
      else {
        val currentExtensions: Seq[BigInt] = {
          val instancesOfMin = Math.floor(Math.log((limit/current).toDouble) / Math.log(multipliers.head)).toInt
          (0 to instancesOfMin) map {x => current * Seq.fill(x)(multipliers.head).product}
        }
        currentExtensions flatMap {ex => extendWith(ex, multipliers.tail)}
      }
    }

    extendWith(number, extensions)
  }

  def countProductsLessThanMax(maxProductInit: BigInt, multipliersInit: Seq[Int]): BigInt = {

    def help(maxProducts: Stream[BigInt], multipliers: Seq[Int]): BigInt = {
      maxProducts map { maxProduct =>
        if (multipliers.isEmpty || maxProduct < multipliers.head) BigInt(1)
        else {
          val current = multipliers.head
          val maxPowerWith = Math.floor(Math.log(maxProduct.toDouble) / Math.log(current)).toInt
          val helpVector = (0 to maxPowerWith) map {pow(current, _)} map { x =>
            maxProduct / x
          }
          help(helpVector.toStream, multipliers.tail)
        }
      } sum
    }

    help(Stream(maxProductInit), multipliersInit)
  }

  def pow(base: Int, exp: Int): BigInt = {
    Seq.fill(exp)(BigInt(base)).product
  }

  println(countProductsLessThanMax(100, Seq(2,3,5,7)))
  println(countProductsLessThanMax(100, Seq(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,91)))
  println(getAllNumberExtensions(1, 100, Seq(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,91)).length)

  assert(getAllNumberExtensions(1, 100, Seq(2, 3, 5,7)).length == 46)
//  println(EulerUtils.fullDecomposeBig(BigInt("1483081367653182781250")))
}
