package euler.tests

/**
  * Created on 23/07/2016.
  */
object SelfPowers extends App {
  def fastExp(x: Int, n: Int): Int = {
    if (n == 0) 1
    else if (n%2 == 0) fastExp(x * x, n / 2)
    else x * fastExp(x * x, (n - 1) / 2)
  }

  def bigFastExp(x: BigInt, n: BigInt): BigInt = {
    if (n == 0) 1
    else if (n%2 == 0) bigFastExp(x * x, n / 2)
    else x * bigFastExp(x * x, (n - 1) / 2)
  }

  assert(fastExp(3, 5) == 243)
  assert(bigFastExp(3, 5) == BigInt(243))

  println(((1 to 1000) map {x => bigFastExp(x, x) % BigInt("10000000000")}).sum)
  //9110846700
}
