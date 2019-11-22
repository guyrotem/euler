package euler.problems

import scala.math.BigDecimal.RoundingMode

/**
  * Created on 30/07/2016.
  */
object E197DivergingFloor {
  def f(x: BigDecimal): BigDecimal = {
    val exp = 30.403243784 - x*x
    BigDecimal(Math.pow(2, exp.toDouble)).setScale(0, RoundingMode.FLOOR) * BigDecimal("0.000000001")
  }

  val diverSeq: Stream[BigDecimal] = BigDecimal(-1) #:: f(-1) #:: (diverSeq zip diverSeq.tail map { x=>
    f((x._1 + x._2) / 2)
  })

  val realSeq: Stream[BigDecimal] = BigDecimal(-1) #:: (diverSeq map f)

  val diverDiff = diverSeq zip diverSeq.tail map {x => x._1 - x._2}
  val realDiff = realSeq zip realSeq.tail map {x => x._1 - x._2}
  val thatsEnoughIndexDive = diverDiff indexWhere {x => BigDecimal(-1e-9) < x && x < BigDecimal(1e-9)}
  val thatsEnoughIndexReal = realDiff indexWhere {x => BigDecimal(-1e-9) < x && x < BigDecimal(1e-9)}
  val xDiverge = realSeq(thatsEnoughIndexDive * 20)

  println(f(xDiverge) - xDiverge)
  println(f(xDiverge) + xDiverge)
  //  fails due to decimal roundings :(
}
