package euler.utils

object EulerUtils {
  def factorial(n: BigInt): BigInt = {
    if (n <= 0) 1 else n * factorial(n-1)
  }


  def naturalsStartingAt(num: Int): Stream[Int] = {
    num #:: naturalsStartingAt(num + 1)
  }
  val naturals: Stream[Int] = 1 #:: (naturals map {_ + 1})
  val bigNaturals: Stream[BigInt] = 1 #:: (bigNaturals map {_ + 1})

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

  def decomposeBig(number: BigInt): Seq[BigInt] = {
    val maxCheck = Math.ceil(Math.sqrt(number.toDouble) + 1).toInt
    def inner(number1: BigInt, factors: Seq[BigInt]): Seq[BigInt] = {
      if (number1 == 1) return factors
      (BigInt(2) until maxCheck).foreach { x =>
        if (number1 % x == 0)
          return inner(number1 / x, factors :+ x)
      }
      factors :+ number1
    }

    inner(number, Seq.empty)
  }

  def fullDecompose(num: Int): Map[Int, Int] = {
    //  More efficient than checking all numbers, apparently
    val decomp = EulerUtils.decompose(num)
    (EulerUtils.decompose(num) map { (x: Int) =>
      (x, decomp count {_ == x})
    }).toSet.toMap
  }

  def fullDecomposeBig(num: BigInt): Map[BigInt, Int] = {
    //  More efficient than checking all numbers, apparently
    val decomp = EulerUtils.decomposeBig(num)
    (EulerUtils.decomposeBig(num) map { (x: BigInt) =>
      (x, decomp count {_ == x})
    }).toSet.toMap
  }

  def gcd(num1: Int, num2: Int): Int = {
    val max = Math.max(num1, num2)
    val min = Math.min(num1, num2)

    if (min == 0) max else gcd(max % min, min)
  }

  def gcdBig(num1: BigInt, num2: BigInt): BigInt = {
    val max = if (num1 < num2) num2 else num1
    val min = if (num1 < num2) num1 else num2

    if (min == 0) max else gcd(max % min, min)
  }


  def shrinkNumbers(n1: Int, n2: Int): (Int, Int) = {
    val gcd1: Int = gcd(n1, n2)

    if (gcd1 == 1) (n1, n2)
    else shrinkNumbers(n1 / gcd1, n2 / gcd1)
  }

  def shrinkNumbersBig(n1: BigInt, n2: BigInt): (BigInt, BigInt) = {
    def gcdBig(smaller: BigInt, bigger: BigInt): BigInt = {
      if (smaller == 0) bigger else gcdBig(bigger % smaller, smaller)
    }

    val gcd1: BigInt = gcdBig(n1, n2)

    if (gcd1 == 1) (n1, n2)
    else shrinkNumbersBig(n1 / gcd1, n2 / gcd1)
  }

  def sqrtPrimeCheck(n: Int): Boolean = {
    ((2 to Math.sqrt(n).toInt) forall {
      n % _ != 0
    }) && n > 1
  }

  def splitToDigits(num: Int): Seq[Int] = {
    num.toString.split("").map{_.toInt}
  }

  def splitToDigits(num: BigInt): Seq[Int] = {
    num.toString.split("").map{_.toInt}
  }

  def readFileFromResources(fileName: String): Seq[String] = {
    scala.io.Source.fromFile(s"src/resources/$fileName").mkString.split("\n")
  }

  def isCoPrime(a: Int, b: Int): Boolean = gcd(a, b) == 1
  def isBigCoPrime(a: BigInt, b: BigInt): Boolean = gcd(a, b) == 1

  private def gcd(a: BigInt, b: BigInt): BigInt = {
    if (b == 0) a else gcd(b, a % b)
  }

  def bigIntSqRootCeil(x: BigInt): BigInt = {
    BigDecimal(scala.math.sqrt(x.doubleValue()).ceil).toBigInt()
  }

  def repeatNTimes[T](f: T => T, in: T, n: Int): T = {
    assert(n >= 0)
    if (n == 0) in
    else repeatNTimes(f, f(in), n - 1)
  }

}


//Triangle.calcOptionsPrism(14743647)

//subsetSum(200, Seq(1,2,5,10,20,50,100,200))

