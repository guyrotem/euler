package euler.tests

import euler.lib.Rational
import euler.utils.EulerUtils

object RationalTest extends App {
  val rat = new Rational(120, 1024)
  rat.shrink()

  EulerUtils.gcd(3, 4)
  EulerUtils.gcd(6, 8)

  for (i <- 1 to 10 if i % 3 == 0) yield i

  "/abc/dsjnfdks/".replaceAll("/[^/]*/$","/ZZZZ/")
}


