package burrows_wheeler

/**
  * Created on 30/07/2016.
  */
object BurrowsWheelerMatchTest extends App {
  val testString = "mississippi"
  val bwTransform = "ipssm$pissii"

  def expect[T](a: T, b: T) = {
    if (a != b) {
      throw new AssertionError(s"$a is not equal to $b")
    }
  }

  expect(BurrowsWheeler.bwMatch(bwTransform, "issi").length, 2)

  val testString2 = "fififfafufififufafefifefafafifefafu"
  val bwTransform2 = "uffffffffeuaefiiaieai$uiaiafffffffff"
  // col1:            $aaaaaeeeffffffffffffffffffiiiiiiuuu
  expect(BurrowsWheeler.bwMatch(bwTransform2, "ife").length, 2)

  println(BurrowsWheeler.bwMatch(bwTransform2, "ife"))
}
