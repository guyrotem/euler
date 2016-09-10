package euler.tests

import euler.utils.{EulerMath, EulerUtils}

/**
  * Created on 16/08/2016.
  */
object E239FoolishPrimes22 extends App {
  val primes = EulerUtils.naturals.tail filter EulerUtils.sqrtPrimeCheck

  val NUMBERS_TOTAL = 100
  val DERANGED_COUNT = 22
  val primesBelow100 = primes take NUMBERS_TOTAL filter {_ < NUMBERS_TOTAL}
  val PRIMES_COUNT = primesBelow100.length

  assert(PRIMES_COUNT == 25)
  assert(reverseCalcDeranged22(11, 4) == calcDeranged22(11, 4))
  assert(reverseCalcDeranged22(5, 1) == 4 * 4  * 3 * 2)
  assert(reverseCalcDeranged22(4, 2) == (8 + 6))

  assert(pick(2, 5) == 10)
  assert(pick(3, 7) == 7 * 6 * 5 / 3 / 2)

  //  should be equiv to reverseCalcDeranged22
  def calcDeranged22(leftTotal: Int, leftSpecial: Int): BigInt = {
    val factorialsCache = (BigInt(0) to NUMBERS_TOTAL) map factorial

    def help(leftTotal: Int, leftSpecial: Int) = {
      if (leftSpecial == 0) factorialsCache(leftTotal)
      else if (leftSpecial == 1) (leftTotal - 1) * factorialsCache(leftTotal - 1)
      else (leftTotal - leftSpecial) * calcDeranged22(leftTotal - 1, leftSpecial - 1) +
        (leftSpecial - 1) * calcDeranged22(leftTotal - 1, leftSpecial - 2)
    }

    help(leftTotal, leftSpecial)
  }

  //  should be equiv to calcDeranged22
  def reverseCalcDeranged22(leftTotal: Int, leftSpecial: Int) = {
    //  init
    val resMap = collection.mutable.Map[(Int, Int), BigInt]()
    (0 to NUMBERS_TOTAL) foreach { x => resMap += (x, 0) -> factorial(x) }
    (1 to NUMBERS_TOTAL) foreach { x => resMap += (x, 1) -> (x - 1) * factorial(x - 1) }
    //  calc
    (2 to leftSpecial) foreach { ls =>
      (ls to NUMBERS_TOTAL) foreach { lt =>
        resMap += (lt, ls) -> ((lt - ls) * resMap(lt - 1, ls - 1) + (ls - 1) * resMap(lt - 1, ls - 2))
      }
    }
    resMap(leftTotal, leftSpecial)
  }

  def factorial(num: BigInt): BigInt = {
    if (num == 0) 1
    else num * factorial(num - 1)
  }

  def factorialRange(max: BigInt, min: BigInt): BigInt = {
    if (min == max) min
    else max * factorialRange(max - 1, min)
  }

  def pick(k: Int, n: Int): BigInt = factorialRange(n, n - k + 1) / factorial(k)

  val deranged22: BigInt = reverseCalcDeranged22(NUMBERS_TOTAL - (PRIMES_COUNT - DERANGED_COUNT), DERANGED_COUNT)
  val totalFoolishOptions = deranged22 * pick(PRIMES_COUNT - DERANGED_COUNT, PRIMES_COUNT)

  val totalOptions = factorial(NUMBERS_TOTAL)
  println(s"$totalFoolishOptions / $totalOptions")

  println(EulerMath.fractionAsDecimalString(totalFoolishOptions, totalOptions, 12))
  //  0.001887854841
}
