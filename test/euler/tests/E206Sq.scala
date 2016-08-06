package euler.tests

/**
  * Created on 02/08/2016.
  */
object E206Sq extends App {
  val start = BigInt(Math.sqrt(1020304050607080900.0).ceil.toInt.toString) / 100
  val stop = BigInt(Math.sqrt (2000000000000000000.0).floor.toInt.toString) / 100

  val pattern = "1[0-9]2[0-9]3[0-9]4[0-9]5[0-9]6[0-9]7[0-9]8[0-9]900"

  println(start)
  println(stop)

  val t = (start to stop) flatMap {y =>
    Seq(y * 100 + 30, y * 100 + 70)
  } find {x =>
    (x * x).toString() matches pattern
  }

  println(s"Result: $t")
  // 1389019170
}
