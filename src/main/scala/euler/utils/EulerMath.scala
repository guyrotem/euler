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

  def solveQuadraticEquation(a: Double, b: Double, c: Double): List[Double] = {
    val delta = Math.sqrt(b*b - 4*a*c)
    List((-b + delta) / (2 * a), (-b - delta) / (2 * a))
  }

  case class Point(x: Double, y: Double) {
    def distanceTo(other: Point): Double = {
      Math.sqrt(Math.pow(x - other.x, 2) + Math.pow(y - other.y, 2))
    }
  }

  def tangentOfLine(from: Point, to: Point): Double = {
    (to.y - from.y) / (to.x - from.x)
  }

  def calcEllipseInnerReflectionTangent(ellipseA: Double, ellipseB: Double)(lineTangent: Double, hitPoint: Point): Double = {
    // Ellipse equation: (x / A)^2 + (y / B)^2 = 1
    // b^2 * x^2 + a^2 * y^2 = a^2*b^2
    // d/dx: 2b^2*x + 2a^2 * y * dy/dx = 0
    //  => dy/dx = -(b/a)^2 * x
    val ellipseTangent = -Math.pow(ellipseB / ellipseA, 2)
    val tangentSlopeDegrees = Math.atan(ellipseTangent * hitPoint.x / hitPoint.y) * 180 / Math.PI
    val currentSlopeDegrees = Math.atan(lineTangent) * 180 / Math.PI

    val diff = tangentSlopeDegrees - currentSlopeDegrees
    val nextReflectionDegrees = tangentSlopeDegrees + 90 + diff
    -1 / Math.tan(nextReflectionDegrees * Math.PI / 180)
  }
}
