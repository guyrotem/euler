package euler.utils

/**
  * Created on 22/07/2016.
  */
trait EulerTestsBase extends App {
  def eulerAssertion[T](actual: T, expected: T): Unit = {
    if (actual != expected) {
      println("€uler assertion failure!")
      println(s"Expected: $expected")
      println(s"Actual: $actual")
    }
  }
}
