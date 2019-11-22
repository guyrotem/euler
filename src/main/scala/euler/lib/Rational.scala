package euler.lib

import euler.utils.EulerUtils

/**
  * Created on 15/07/2016.
  */

case class Rational(n: Int, d: Int) {

  def simpleAdd(other: Rational): Rational = {
    new Rational(
      n * other.d + d * other.n,
      d * other.d
    )
  }

  def add(other: Rational): Rational = {
    simpleAdd(other).shrink()
  }

  def shrink(): Rational = {
    val shrunkList = EulerUtils.shrinkNumbers(n, d)
    new Rational(shrunkList._1, shrunkList._2)
  }

  override def toString: String = {
    n + " / " + d
  }
}
