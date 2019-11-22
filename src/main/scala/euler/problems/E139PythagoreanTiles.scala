package euler.problems

import euler.utils.EulerUtils

/**
  * Created on 30/07/2016.
  */
object E139PythagoreanTiles {
  def createPythagoreanTriple(m: Int, n: Int): (Int, Int, Int) = {
    (m*m - n*n, 2*m*n, m*m + n*n)
  }
  //  https://en.wikipedia.org/wiki/Pythagorean_triple

  //  CONDITIONS:
  //  m > n, m-n € N_odd, gcd(m, n) == 1  //  Euclid
  //  c % +-(b - a) = 0 <=> (m^2 + n^2) % (n^2 + 2mn - m^2) = 0 //  "Tiles"
  //  sum = m^2 - n^2 + 2mn + m^2 + n^2 = 2m(m+n) < X //  Constraint

  assert(createPythagoreanTriple(2, 1) == (3,4,5))
  assert(createPythagoreanTriple(3, 2) == (5, 12, 13))
  assert(createPythagoreanTriple(5, 2) == (21, 20, 29))
  assert(createPythagoreanTriple(8, 3) == (55, 48, 73))

  val MAX_PERIMETER = 100000000

  //  2m(m+n)<X => m+n < X/2m => n < X/2m - m
  //  m ~= X/a => 0 < a/2 - X/a => a^2 < 2X => a < SQRT(2X) = 14142. ...

  val m = 2 to 14142

  def makeN(m: Int) = {

    val limit1 = m-1  //  n > m
    val limit2 = (MAX_PERIMETER.toDouble/(2*m) - m).floor.toInt
    val limit = Math.min(limit1, limit2)

    val startPoint = m % 2 + 1

    (m, startPoint.to(limit, 2))
  }

  val optionalTriangles = m.toStream map makeN

  def trianglePerimeter(m: Int, n: Int) = {
    createPythagoreanTriple(m, n) match {
      case (a, b, c) => a+b+c
    }
  }

  private def numberOfTrianglesFromTriple(m: Int, n: Int, max: Int = MAX_PERIMETER): Int = {
    max / trianglePerimeter(m, n)
  }

  val resultPerM = optionalTriangles map {
    case (mm: Int, ns: Seq[Int]) => (ns filter isCoPrime(mm) filter isTileable(mm) map {
      nn => numberOfTrianglesFromTriple(mm, nn)
    }).sum
  }

  private def isCoPrime(x: Int)(y: Int): Boolean = {
    EulerUtils.isCoPrime(x, y)
  }

  private def isTileable(x: Int)(y: Int): Boolean = {
    //    (m^2 + n^2) % (n^2 + 2mn - m^2)
    (x*x + y*y) % (y*y + 2*x*y - x*x) == 0
  }

  println(resultPerM.sum)
  // 10057761
}
