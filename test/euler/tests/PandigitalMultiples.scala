package euler.tests
/**
  * Created on 22/07/2016.
  */
object PandigitalMultiples extends App {
  def multiplyOneToN(n: Int): Int = {
    val source = (1 to 9) map {_ * n}
    val a = source map {_.toString}
    a.foldRight("")({_ + _}).substring(0, 9).toInt
  }

  def isPandigital(num: Int): Boolean = {
    (1 to 9) map {_.toString} forall {num.toString.contains(_)}
  }

  assert(multiplyOneToN(192) == 192384576)

  val panDig = (1 to 9999) zip ((1 to 9999) map multiplyOneToN) filter {x => isPandigital(x._2)}
  println(panDig.max)
  println(multiplyOneToN(192))
}
