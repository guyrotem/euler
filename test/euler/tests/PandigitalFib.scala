package euler.tests

import scala.collection.immutable.IndexedSeq

/**
  * Created on 16/07/2016.
  */
object PandigitalFib extends App {

  def isPanDigital(digits: IndexedSeq[Int]): Boolean = {
    (1 to 9) forall { digits contains _ }
  }

  def isPandigitalStartAndEnd(num: BigInt): Boolean = {
    val s = num.toString.split("").map{_.toInt}
    if (s.length < 18) return false
    val first = (0 to 8) map {index => s(index)}
    val last = (0 to 8) map {index => s(s.length - 1 - index)}

    isPanDigital(first) && isPanDigital(last)
  }

  assert(isPandigitalStartAndEnd(BigInt("1325476890000732824792389423700978654231")))
  assert(!isPandigitalStartAndEnd(BigInt("131")))
  assert(!isPandigitalStartAndEnd(BigInt("1223456789000000987654321")))

  val fibs: Stream[BigInt] = BigInt(1) #:: BigInt(1) #:: ((fibs zip fibs.tail) map { x => x._1 + x._2 })

//  println(fibs take 1000 map {_.length} toList)
  val f = fibs indexWhere isPandigitalStartAndEnd
//  println(fibs(329468))
//  println(f)
}