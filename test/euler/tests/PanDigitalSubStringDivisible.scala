package euler.tests

/**
  * Created on 23/07/2016.
  */
object PanDigitalSubStringDivisible extends App {
  def is09PanDigital(num: BigInt): Boolean = {
    num.toString.length == 10 && ((0 to 9) forall { x => num.toString().contains(x.toString) })
  }

  assert(is09PanDigital(BigInt("2091873645")))
  assert(!is09PanDigital(BigInt("291873645")))
  assert(!is09PanDigital(BigInt("29018731645")))

  val limit3digits = 999
  def allDivisibleByN(N: Int, limit: Int = limit3digits) = (1 to limit / N) map { _ * N}

  def idgafDivisibilityProperty(num: BigInt): Boolean = {
    val numStr = num.toString()
    numStr.substring(3, 4).toInt % 2 == 0 &&
      numStr.substring(2, 5).toInt % 3 == 0 &&
      numStr.substring(5, 6).toInt % 5 == 0 &&
      numStr.substring(4, 7).toInt % 7 == 0 &&
      numStr.substring(5, 8).toInt % 11 == 0 &&
      numStr.substring(6, 9).toInt % 13 == 0 &&
      numStr.substring(7).toInt % 17 == 0
  }

  assert(idgafDivisibilityProperty(BigInt(1406357289)))

  def checkHafifa(xnRight: Int) = {
    (xnLeft: Int) => "%03d".format(xnRight).toString.substring(0, 2) == "%03d".format(xnLeft).substring(1)
  }

  val results: Seq[BigInt] =
    allDivisibleByN(17) flatMap { x17 =>
      allDivisibleByN(13) filter checkHafifa(x17) flatMap { x13 =>
        allDivisibleByN(11) filter checkHafifa(x13) flatMap { x11 =>
          allDivisibleByN(7) filter checkHafifa(x11) flatMap { x7 =>
            allDivisibleByN(5) filter checkHafifa(x7) flatMap { x5 =>
              allDivisibleByN(3) filter checkHafifa(x5) flatMap { x3 =>
                allDivisibleByN(2) filter checkHafifa(x3) map { x2 =>
                  val str =
                    "%03d".format(x2).substring(0, 1) +
                      "%03d".format(x3).substring(0, 1) +
                      "%03d".format(x5).substring(0, 1) +
                      "%03d".format(x7).substring(0, 1) +
                      "%03d".format(x11).substring(0, 1) +
                      "%03d".format(x13).substring(0, 1) +
                      "%03d".format(x17)
                  BigInt(str)
                }
              }
            }
          }
        }
      }
    }
  val leftDigit = (1 to 9) map {_.toString}
  val withLeftDigit = leftDigit flatMap { d => results map { res => BigInt(d + res) } }
  val panDigOnly = withLeftDigit filter is09PanDigital
  println(panDigOnly)
  println(panDigOnly.sum)
  //  Vector(1460357289, 1406357289, 1430952867, 4160357289, 4106357289, 4130952867)
  //  16695334890
}
