package euler.tests

import euler.utils.Utils

/**
  * Created on 16/07/2016.
  */
object ShrinkableFractions extends App {
  def shrinkText(a: Int, b: Int) = {
    val aDigits = a.toString.split("")
    val bDigits = b.toString.split("")

    val aFilt = aDigits filter { x => !bDigits.contains(x) }
    val bFilt = bDigits filter { x => !aDigits.contains(x) }

    if (aFilt.length == 1 && bFilt.length == 1) shrinkGcd(aFilt.head.toInt, bFilt.head.toInt)
    else (0, 0)
  }

  def shrinkGcd(a: Int, b: Int) = {
    (a / Utils.gcd(a, b), b / Utils.gcd(a, b))
  }

  val allFracs = for {
    i <- 10 to 99
    j <- i+1 to 99
  } yield (i, j)

  val res = allFracs filter {
    case (a, b) => shrinkGcd(a, b) == shrinkText(a, b) && (a%10) != 0
  }

  val product = shrinkGcd((res map {_._1}).product, (res map {_._2}).product)
  println(product)
}
