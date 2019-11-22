package euler.problems

/**
  * Created on 14/08/2016.
  */
object E120SquareRemainders extends App {
  //  [(a - 1)^n + (a + 1)^n] % (a^2)
  //  n odd: 2an % a^2
  //  n even: 2 % a^2

  def maxR(a: Int): Int = (1 until a) map {n => (2 * n * a) % (a*a)} max
  def maxR2(a: Int): Int = a * ((a - 1) / 2 * 2)

  assert(maxR(7) == 42)

  println((3 to 1000) map maxR2 sum)
  //333082500
}
