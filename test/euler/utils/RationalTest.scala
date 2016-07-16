package euler.utils

import euler.lib.Rational



object RationalTest extends App {
  val rat = new Rational(120, 1024)
  rat.shrink()

  Utils.gcd(3, 4)
  Utils.gcd(6, 8)

  for (i <- 1 to 10 if i % 3 == 0) yield i

  "/abc/dsjnfdks/".replaceAll("/[^/]*/$","/ZZZZ/")
}


