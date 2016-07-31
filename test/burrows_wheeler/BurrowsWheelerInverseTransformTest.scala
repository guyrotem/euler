package burrows_wheeler

/**
  * Created on 29/07/2016.
  */
object BurrowsWheelerInverseTransformTest extends App {
  val testString = "panamabananas"
  val expectedResult = "smnpbnnaaaaa$a"

  assume(BurrowsWheeler.inverseBwtNaive(expectedResult) == testString, s"BWT failed on $testString. expected: $testString, actual ${BurrowsWheeler.inverseBwtNaive(expectedResult)}")
  assume(BurrowsWheeler.inverseBwt("smnpbnnaaaaa$a") == "panamabananas")
}
