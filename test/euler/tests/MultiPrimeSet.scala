package euler.tests

//5701, 8389, 13, 5197, 6733

case class MultiPrimeSet(
                          primesSet: Set[Set[Int]],
                          level: Int,
                          source: Seq[Int]
                        )