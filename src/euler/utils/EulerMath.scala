package euler.utils

/**
  * Created on 31/07/2016.
  */
object EulerMath {
  trait BigFraction {
    val n: BigInt
    val d: BigInt

    def +(other: BigFraction): BigFraction
    def -(other: BigFraction): BigFraction
    def *(other: BigFraction): BigFraction
    def /(other: BigFraction): BigFraction

    def one = CreateFraction(1, 1)
  }

  private case class BigFractionImpl(n: BigInt, d: BigInt) extends BigFraction {
    def +(other: BigFraction): BigFraction = {
      shrink(n * other.d + d * other.n, d * other.d)
    }

    def -(other: BigFraction): BigFraction = {
      shrink(n * other.d - d * other.n, d * other.d)
    }

    def *(other: BigFraction): BigFraction = {
      shrink(n * other.n, d * other.d)
    }

    def /(other: BigFraction): BigFraction = {
      shrink(n * other.d, d * other.n)
    }

    private def shrink(nn: BigInt, dd: BigInt): BigFraction = {
      val gcd = EulerUtils.gcdBig(nn, dd)
      BigFractionImpl(nn / gcd, dd / gcd)
    }

    override def toString = {
      assert(d != 0, "Denominator cannot be 0!")
      if (d == 1) n.toString() else s"$n / $d"
    }
  }

  def CreateFraction(n: BigInt, d: BigInt): BigFraction = {
    val gcd = EulerUtils.gcdBig(n, d)
    BigFractionImpl(n / gcd, d / gcd)
  }

  def totient(num: BigInt): BigInt = {
    //  see TOTIENT FUNCTION
    val primeFactors = EulerUtils.fullDecomposeBig(num).keys
    num * (primeFactors map { _ - 1 }).product / primeFactors.product
  }

  def fractionAsDecimalString(n: BigInt, d: BigInt, precision: Int = 9): String = {
    val whole = n / d

    var temp = n % d
    var out = whole.toString() + "."
    var l = precision

    while (l > 0) {
      out += (10 * temp) / d
      temp = (10 * temp) % d
      l -= 1
    }

    out
  }
}
