package euler.utils

import java.util.Date

object Utils {
  val naturals: Stream[Int] = 1 #:: (naturals map {_ + 1})

  def calcOptionsPrism(numEdges: Int): Int = {
    if (numEdges % 2 == 0) return 0

    val level = (numEdges + 3) / 2

    solveLevel(level)
  }

  private def solveLevel(level: Int): Int = {
    val startTime = new Date().getTime

    def getTime = {
      new Date().getTime - startTime
    }

    println(s"level: $level @$getTime")
    val factors = decompose(level)
    println(s"decomposing done @$getTime")

    val exitCount: Int = (level + 1)/3
    val cVector = naturals map {x => 3*x - (level % 3)} take exitCount


    println(s"vector created: @$getTime")
    //  val count = cVector.count(isCoPrime(level, _))
    val count = cVector.count { (divisor: Int) =>
      factors forall { factor: Int =>
        divisor % factor != 0
      }
    }
    println(s"end time: @$getTime")
    count
  }

  def decompose(number: Int): Seq[Int] = {
    val maxCheck = Math.ceil(Math.sqrt(number) + 1).toInt
    def inner(number1: Int, factors: Seq[Int]): Seq[Int] = {
      if (number1 == 1) return factors
      (2 until maxCheck).foreach { x =>
        if (number1 % x == 0)
          return inner(number1 / x, factors :+ x)
      }
      factors :+ number1
    }

    inner(number, Seq.empty)
  }

  def fullDecompose(num: Int): Map[Int, Int] = {
    //  More efficient than checking all numbers, apparently
    val decomp = Utils.decompose(num)
    (Utils.decompose(num) map { (x: Int) =>
      (x, decomp count {_ == x})
    }).toSet.toMap
  }

  def gcd(num1: Int, num2: Int): Int = {
    val max = Math.max(num1, num2)
    val min = Math.min(num1, num2)

    if (min == 0) max else gcd(max % min, min)
  }

  def shrinkNumbers(n1: Int, n2: Int): (Int, Int) = {
    val gcd1: Int = gcd(n1, n2)

    if (gcd1 == 1) (n1, n2)
    else shrinkNumbers(n1 / gcd1, n2 / gcd1)
  }

  def shrinkNumbersBig(n1: BigInt, n2: BigInt): (BigInt, BigInt) = {
    def gcdBig(smaller: BigInt, bigger: BigInt): BigInt = {
      if (smaller == 0) bigger else gcd(bigger % smaller, smaller)
    }

    val gcd1: BigInt = gcdBig(n1, n2)

    if (gcd1 == 1) (n1, n2)
    else shrinkNumbersBig(n1 / gcd1, n2 / gcd1)
  }

  def sqrtPrimeCheck(n: Int): Boolean = {
    (2 to Math.sqrt(n).toInt) forall {
      n % _ != 0
    }
  }

  def splitToDigits(num: Int): Seq[Int] = {
    num.toString.split("").map{_.toInt}
  }

  private def isCoPrime(a: BigInt, b: BigInt): Boolean = gcd(a, b) == 1

  private def gcd(a: BigInt, b: BigInt): BigInt = {
    if (b == 0) a else gcd(b, a % b)
  }


  ////


}


//Triangle.calcOptionsPrism(14743647)

//subsetSum(200, Seq(1,2,5,10,20,50,100,200))

