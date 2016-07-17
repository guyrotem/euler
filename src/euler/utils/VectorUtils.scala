package euler.utils

/**
  * Created on 16/07/2016.
  */
object VectorUtils {
  case class Coordinate(x: Int, y: Int)
  case class Vector(x: Int, y: Int) {
    def angleRad: Double = {
      Math.atan2(y, x)
    }

    def angleDeg: Double = {
      angleRad * 180 / Math.PI
    }
  }
  case class Triangle(a: Coordinate, b: Coordinate, c: Coordinate) {
    def vertices: Seq[Coordinate] = Seq(a, b, c)
  }
}
