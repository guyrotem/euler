package euler.tests

/**
  * Created on 22/07/2016.
  */
object IntegerRightAngleTriangle extends App {
  def getAllSolutions(perimeter: Int): Seq[(Int, Int, Int)] = {
    for {
      c <- 3 until perimeter
      b <- 1 until perimeter - c
      a = perimeter - c - b
      if a * a + b * b == c * c && a < b
    } yield (a, b, c)
  }

  assert(getAllSolutions(120).length == 3)

  val vector = 1 to 1000
  val results = vector map getAllSolutions map {_.length}
  val max = results.max
  val maxIndex = results indexOf max
  println(s"$max is in index ${maxIndex + 1}")
}
