package euler.tests

import euler.utils.EulerUtils

/**
  * Created on 23/07/2016.
  */
object PrimePermutations extends App {
  def isPP(num1: Int): Boolean = {
    val validPermutations = (createAllNumberPermutations(num1) filter EulerUtils.sqrtPrimeCheck).sorted

    val seqStart = validPermutations find { num2 =>
      //  num3 - num2 == num2 - num1
      (validPermutations contains (2*num2 - num1)) && (num2 > num1)
    }
//    seqStart foreach { x=> println(s"$num -> $x -> ${2*x - num}")}
    seqStart.isDefined
  }

  def createAllNumberPermutations(num: Int): Seq[Int] = {
    createAllPermutations(num.toString) map {_.toInt}
  }

  def createAllPermutations(str: String): Seq[String] = {
    if (str.length == 0) Seq(str)
    else (0 until str.length) flatMap {index =>
      createAllPermutations(str.substring(0, index) + str.substring(index + 1)) map {str(index) + _}
    }
  }

  assert(createAllPermutations("1") == Seq("1"))
  assert(createAllPermutations("12") == Seq("12", "21"))

  assert(createAllNumberPermutations(456) == Seq(456, 465, 546, 564, 645, 654))
  assert(isPP(1487))

  println((1001 to 9999) filter EulerUtils.sqrtPrimeCheck filter isPP)


}
