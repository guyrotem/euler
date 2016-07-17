package euler.tests

/**
  * Created on 18/07/2016.
  */
object BinarDecimalPalindromes extends App {

  def isPalindrome(num: String): Boolean = num.toString == num.toString.reverse
  def isPalindrome(num: Int): Boolean = isPalindrome(num.toString)

  def toBinary(num: Int): String = num.toBinaryString

  val allPs = (1 to 999999) filter isPalindrome filter { x => isPalindrome(x.toBinaryString) }

  println(allPs.sum)
}


