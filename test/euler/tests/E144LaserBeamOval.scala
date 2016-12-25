package euler.tests

import euler.utils.EulerMath
import euler.utils.EulerMath.Point

/**
  * Created on 21/09/2016.
  */
object E144LaserBeamOval extends App {
  //  4x^2 + y^2 = 100
  //  m = -4x/y
  private def calcIntersectionsWithEllipse(m: Double, n: Double): List[Point] = {
    //  y = mx + n
    //  4x^2 + y^2 = 100
    //  (x / 5)^2 + (y / 10)^2 = 1
    //  => 4x^2 + (mx + n)^2 = 100
    //  =>  (4 + m^2)x^2 + (2mn)x + (n^2 - 100) = 0
    //  x = (- 2 * m * n ± sqrt(2*m^2*n^2 - 4 * (n^2 - 100)(4 + m^2))) / 2(4+m^2)
    val a = 4 + m*m
    val b = 2*m*n
    val c = n*n - 100

    EulerMath.solveQuadraticEquation(a, b, c) map calcPointOfLineAtX(m, n)
  }

  def calcPointOfLineAtX(m: Double, n: Double)(x: Double): Point = {
    Point(x, m * x +  n)
  }

  def e144EllipseReflectionCalc = EulerMath.calcEllipseInnerReflectionTangent(5, 10) _

  def getPointFarFrom(intersections: List[Point], originalIntersection: Point): Point = {
    intersections maxBy originalIntersection.distanceTo
  }

  def calcNextIntersection(m: Double, hitPoint: Point): Point = {
    val nextSlope = e144EllipseReflectionCalc(m, hitPoint)
    val intersections = calcIntersectionsWithEllipse(nextSlope, hitPoint.y - nextSlope * hitPoint.x)
    intersections maxBy hitPoint.distanceTo
  }

  assume(getPointFarFrom(List(Point(1, 1), Point(3, 2), Point(8, 7), Point(4, 4.3)), Point(5, 5)) == Point(1, 1))

  val startPoint = Point(0, 10.1)
  val nextPoint = Point(1.4, -9.6)

  def getNextLaser(currentLaser: Laser): Laser = {
    val nextTangent = e144EllipseReflectionCalc(currentLaser.m, currentLaser.point)
    val nextPoint = calcNextIntersection(currentLaser.m, currentLaser.point)
    Laser(nextTangent, nextPoint)
  }

  lazy val laserStream: Stream[Laser] = Laser(EulerMath.tangentOfLine(startPoint, nextPoint), nextPoint) #:: (laserStream map getNextLaser)


  def isOpenEllipsePart(point: Point): Boolean = {
    Math.abs(point.x) < 0.01 && point.y > 0
  }

  println(laserStream map {_.point} indexWhere isOpenEllipsePart)
  //  354
}

case class Laser(m: Double, point: Point)