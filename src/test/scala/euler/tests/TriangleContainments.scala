package euler.tests

import euler.utils.EulerUtils
import euler.utils.VectorUtils._

/**
  * Created on 16/07/2016.
  */
object TriangleContainments extends App {

  def isContained(triangle: Triangle, coordinate: Coordinate): Boolean = {
    val allAngles = (triangle.vertices map {getAngle(coordinate, _)}).sorted
    val diffs = (allAngles zip (allAngles :+ (allAngles.head + 360)).tail) map {
      case (current, next) => next - current
    }
    diffs forall {_ < 180}
  }

  val origin = Coordinate(0, 0)

  def isTriangleContainsOrigin(triangle: Triangle): Boolean = {
    isContained(triangle, origin)
  }

  def getAngle(coordinate1: Coordinate, coordinate2: Coordinate): Double = {
    Vector(coordinate2.x - coordinate1.x, coordinate2.y - coordinate1.y).angleDeg
  }

  assert(getAngle(Coordinate(1, 1), Coordinate(2, 2)) == 45)
  assert(getAngle(Coordinate(0, 0), Coordinate(0, 1)) == 90)

  val triangle1 = Triangle(Coordinate(-340,495), Coordinate(-153,-910), Coordinate(835,-947))
  val triangle2 = Triangle(Coordinate(-175,41), Coordinate(-421,-714), Coordinate(574,-645))

  assert(isContained(triangle1, origin))
  assert(!isContained(triangle2, origin))

  var demoTriangles = EulerUtils.readFileFromResources("triangles1.txt")

  def buildTriangleFromString(str: String): Triangle = {
    val coordinates = str.split(",") map {_.toInt}
    assert(coordinates.length == 6)

    Triangle(
      Coordinate(coordinates(0), coordinates(1)),
      Coordinate(coordinates(2), coordinates(3)),
      Coordinate(coordinates(4), coordinates(5))
    )
  }

  val trianglesContainOrigin = demoTriangles map buildTriangleFromString count isTriangleContainsOrigin
  println(trianglesContainOrigin)
}
