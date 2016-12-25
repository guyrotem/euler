package euler.tests

import euler.utils.EulerUtils

/**
  * Created on 23/09/2016.
  */
object E233LatticeCirclePoints extends App {
  //  circle: center(N/2, N/2), radius(SQRT(2)/2 * N)
  //  (x - N/2)^2 + (y - N/2)^2 = N^2/2
  //  S=x-N/2, T=y-N/2, M=N/2 (N is even)
  //  S^2 + T^2 = 2M^2
  //  Use Pythagorean triples
//  def bruteCountPythagoreanSolutions(N: BigInt): BigInt = {
//    val maxSearch = N / 2 - 1
//    val countRoots = (BigInt(1) to maxSearch).count { x => isSquare(N*N/2 - x * x) }
//    4 * (2 * countRoots + 1)
//  }

  def countPythagoreanSolutions(N: BigInt): Int = {
    val bs = EulerUtils.fullDecomposeBig(N * N / 2) filter {
      //  NOTE: if any base where (base % 4 == 3) has an ODD exp, the result is 0
      //  BUT: we are only testing numbers of the form 2 * m^2, therefore all exps are even!
      case (base, _) => base % 4 == 1
    } map {
      case (_, exp) => exp + 1
    }
    4 * bs.product
  }

//  def isSquare(x: BigInt): Boolean = {
//    if (x <= 0) false
//    else {
//      val pseudoRoot = EulerUtils.bigIntSqRootCeil(x)
//      pseudoRoot * pseudoRoot == x
//    }
//  }

  def measureTime[T](f: => Unit => T): T = {
    val startTime = System.currentTimeMillis()
    val t = f()
    println(System.currentTimeMillis() - startTime)
    t
  }

//  assert(bruteCountPythagoreanSolutions(10000) == 36, bruteCountPythagoreanSolutions(10000))
  assert(countPythagoreanSolutions(10000) == 36, countPythagoreanSolutions(10000))
  assert(countPythagoreanSolutions(2210) == 108, countPythagoreanSolutions(2210))
  assert(countPythagoreanSolutions(BigInt("8302970")) == 420, countPythagoreanSolutions(BigInt("8302970")))
  assert(countPythagoreanSolutions(BigInt("718250")) == 420, countPythagoreanSolutions(BigInt("718250")))

  //val rangeEnd = "100000000000"

//  val count2 = measureTime { _ =>
//    (BigInt(1) to BigInt("10000") / 2) count { x => countPythagoreanSolutions(x) == 420 }
//  }

  //(1 to 150) foreach { x => println(s"${2*x}: ${countPythagoreanSolutions(2 * x)}, ")}

  //  N <- 1 to 1e11
  //  N^2 / 2 <- 2 to 5e21
  //  of the form 2^(2l-1) * PI[(4ki + 3)^2mi] * (4k1 + 1)^2 * (4k2 + 1)^4 * (4k3 + 1)^6]
  //  or:         2^(2l-1) * PI[(4ki + 3)^2mi] * (4k1 + 1)^14 * (4k2 + 1)^6]
  //  or:         2^(2l-1) * PI[(4ki + 3)^2mi] * (4k1 + 1)^20 * (4k2 + 1)^4]
  //  or:         2^(2l-1) * PI[(4ki + 3)^2mi] * (4k1 + 1)^34 * (4k2 + 1)^2]
  //  or:         2^(2l-1) * PI[(4ki + 3)^2mi] * (4k1 + 1)^104]

  //  Primes we care about are in range: (4k+3)^2 < 5e21 / (2 * 17^2 * 13^4 * 5^6)
  //  Primes we care about are in range: (4k+1)^2 < 5e21 / (2 * 13^4 * 5^6)
  //  4k+3 <= 139228
  //  4k+1 <= 2366864

  val primesInRange = measureTime { _ =>
    (2 to 2366864) filter EulerUtils.sqrtPrimeCheck
  }

  val fourK1 = primesInRange filter { _ % 4 == 1 } //86929
  //  5, 13, 17, 29, 37, 41, ...
  val fourK3 = primesInRange filter { _ % 4 == 3 } //87111
  //  3, 7, 11, 19, 23, 31, 43, ...

  val maxN2 = BigInt("5000000000000000000000")

  def expandBases(limit: BigInt)(base420: Base420): List[Base420] = {
    List(
      Base420(base420.pow2, base420.pow4, base420.pow6),
      Base420(base420.pow2, base420.pow6, base420.pow4),
      Base420(base420.pow4, base420.pow2, base420.pow6),
      Base420(base420.pow4, base420.pow6, base420.pow2),
      Base420(base420.pow6, base420.pow2, base420.pow4),
      Base420(base420.pow6, base420.pow4, base420.pow2)
    ) filter {_.asNumber <= limit}
  }

  def findBase420s(): List[Base420] = {
    val fourK1Stream = fourK1.toStream

    def makeTripletStream(s: Stream[Int], limit: BigInt): Stream[(Int, Int, Int)] = {
      if (s.isEmpty || s.tail.isEmpty || s.tail.tail.isEmpty || Base420(s(2), s(1), s(0)).asNumber > limit) Stream.empty
      else {
        (makeDoubleStream(s.tail, limit, s.head) map {
          res => (s.head, res._1, res._2)
        }) #::: makeTripletStream(s.tail, limit)
      }
    }

    def makeDoubleStream(s: Stream[Int], limit: BigInt, small: Int): Stream[(Int, Int)] = {
      if (s.isEmpty || s.tail.isEmpty || Base420(s(1), s(0), small).asNumber > limit) Stream.empty
      else {
        (s.tail map {(s.head, _)} filter { x =>
          Base420(x._2, x._1, small).asNumber <= limit
        }) #::: makeDoubleStream(s.tail, limit, small)
      }
    }

    //232470 items
    val bases = makeTripletStream(fourK1Stream, maxN2) map { l => Base420(l._3, l._2, l._1) }
    //309239 items
    bases flatMap expandBases(maxN2) toList
  }

  case class Base420(pow2: Int, pow4: Int, pow6: Int) {
    def asNumber: BigInt = {
      2 *
        Seq.fill(2)(BigInt(pow2)).product *
        Seq.fill(4)(BigInt(pow4)).product *
        Seq.fill(6)(BigInt(pow6)).product
    }
  }

  //139228 is ~the maximum complement for N=1e11
  val multipliersBase = (2 +: fourK3) filter {_ <= 139228} map { x => x * x}

  def countProductsLessThanMax(maxProductInit: BigInt, multipliersInit: Seq[Int]): BigInt = {

    def help(maxProducts: Stream[BigInt], multipliers: Seq[Int]): BigInt = {
      val helpStream = maxProducts.foldLeft(Stream.empty.asInstanceOf[Stream[BigInt]]) { (str: Stream[BigInt], maxProduct: BigInt) =>
        if (multipliers.isEmpty || maxProduct < multipliers.head) Stream(BigInt(1))
        else {
          val current = multipliers.head
          val maxPowerWith = Math.floor(Math.log(maxProduct.toDouble) / Math.log(current)).toInt
          val helpVector = (0 to maxPowerWith) map {pow(current, _)} map { y =>
            maxProduct / y
          }
          str #::: helpVector.toStream
         }
      }
      help(helpStream, multipliers.tail)
    }

    help(Stream(maxProductInit), multipliersInit)
  }

  def pow(base: Int, exp: Int) = {
    Seq.fill(exp)(BigInt(base)).product
  }

  val base420s = findBase420s()
  println(base420s.length)

  val total = (base420s map {
    x => countProductsLessThanMax(maxN2 / x.asNumber, multipliersBase.toStream)
  }).sum
  println(total)

}
