package euler.tests

/**
  * Created on 08/08/2016.
  */
object E435PolynomialFib extends App {
  val mod = BigInt("1307674368000")

  lazy val fibStream: Stream[BigInt] = 1 #:: 1 #:: (fibStream zip fibStream.tail map {case(a, b) => (a + b) % mod})

  def polyStream(x: Int): Stream[BigInt] = {
    lazy val polys: Stream[BigInt] = x #:: (polys map {last => (last * x) % mod})
    polys
  }

  def getPolyFibStream(x: Int): Stream[BigInt] = {
    fibStream zip polyStream(x) map {case (a, b) => (a * b) % mod}
  }

  def sumStream(stream: Stream[BigInt], amount: BigInt): BigInt = {
    var sum = BigInt(0)
    var counter = BigInt(0)
    var current = stream
    while (counter < amount) {
      sum = (sum + current.head) % mod
      current = current.tail
      counter += 1
    }
    sum
  }

//  val amount = BigInt("1000000000000000")
  val amount = BigInt("1000")

  val x1 = getPolyFibStream(1)
  val x2 = getPolyFibStream(2)

  println(sumStream(x2, amount))
}
