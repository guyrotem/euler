package euler.tests

import euler.utils.EulerUtils

import scala.collection.immutable.IndexedSeq

/**
  * Created on 22/07/2016.
  */
object Champernowne extends App {

  def sumPartial(lengthPerBin: IndexedSeq[Int], numElements: Int): Int = {
    ((0 until numElements) map {lengthPerBin(_)}).sum
  }


  def getNthChampernowne(n: Int): Int = {

    val numsPerLength = (1 to 7) map {n => (9 * Math.pow(10, n-1)).toInt}
    val lengthPerBin = (numsPerLength zip EulerUtils.naturals) map {x => x._1 * x._2}
    val cumBin = (0 to 7) map {sumPartial(lengthPerBin, _)}
    val cell = cumBin indexWhere {(x: Int) => n <= x}
    val remain = n - 1 - cumBin(cell - 1)
    val numIndex = remain / cell
    val numDigit = remain % cell
    val cellValues = Math.pow(10, cell - 1).toInt until Math.pow(10, cell).toInt
    cellValues(numIndex).toString()(numDigit).toInt - 48
  }

  assert(getNthChampernowne(1) == 1)
  assert(getNthChampernowne(2) == 2)
  assert(getNthChampernowne(11) == 0)
  assert(getNthChampernowne(12) == 1)

  val product = ((0 to 6) map {Math.pow(10, _).toInt} map getNthChampernowne).product

  println(product)
}
