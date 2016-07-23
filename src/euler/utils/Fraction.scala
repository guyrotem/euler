package euler.utils

/**
  * Created on 23/07/2016.
  */
class Fraction(numer: BigInt, denum: BigInt) {
  private val smaller = if (numer < denum) numer else denum
  private val bigger = if (numer < denum) denum else numer
  private val shrunk = EulerUtils.shrinkNumbersBig(smaller, bigger)
  val numer1: BigInt = if (numer < denum) shrunk._1 else shrunk._2
  val denum1: BigInt = if (numer < denum) shrunk._2 else shrunk._1

  def +(o: Fraction): Fraction = {
    new Fraction(numer1 * o.denum1 + o.numer1 * denum1, denum1 * o.denum1)
  }

  def -(o: Fraction): Fraction = {
    new Fraction(numer1 * o.denum1 - o.numer1 + denum1, denum1 * o.denum1)
  }

  def *(o: Fraction): Fraction = {
    new Fraction(numer1 * o.numer1, denum1 * o.denum1)
  }

  def /(o: Fraction): Fraction = {
    new Fraction(numer1 * o.denum1, denum1 * o.numer1)
  }

  def invert: Fraction = {
    new Fraction(denum1, numer1)
  }

  override def toString: String = {
    s"$numer1 / $denum1"
  }

  override def equals(o: Any): Boolean = {
    o match {
      case x: Fraction => x.numer1 == numer1 && x.denum1 == denum1
      case _ => false
    }
  }

}
object FractionImplicits {

  implicit class `Int --> Fraction`(intValue: Int) {
    def asFraction: Fraction = new Fraction(intValue, BigInt(1))
  }

}
