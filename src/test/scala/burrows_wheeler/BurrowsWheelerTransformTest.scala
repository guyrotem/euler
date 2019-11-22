package burrows_wheeler

/**
  * Created on 29/07/2016.
  */
object BurrowsWheelerTransformTest extends App {
  val testString = "panamabananas"
  val expectedResult = "smnpbnnaaaaa$a"

  assert(BurrowsWheeler.bwtNaive(testString) == "smnpbnnaaaaa$a", s"BWT failed on $testString. expected: $expectedResult, actual: ${BurrowsWheeler.bwtNaive(testString)}")
}
