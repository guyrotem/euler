package euler.problems

import euler.utils.EulerMath

/**
  * Created on 03/08/2016.
  */
object E214TotientChains extends App {

  def createTotientChainBrute(num: BigInt): Seq[BigInt] = {
    if (num == 1) Seq(1)
    else num +: createTotientChainBrute(EulerMath.totient(num))
  }

  assume(createTotientChainBrute(5).map(_.intValue) == Seq(5,4,2,1))

//  val totientChains = (BigInt(1) to 2048) map createTotientChainBrute

//  totientChains map {_.length} indexOf(K) > 2^(K-2)

  val b1024 = BigInt(1024)
  val b2pow22 = b1024 * b1024 * 4 //  minimum for 25 chain length

  val chainLengths = scala.collection.mutable.Map(BigInt(1) -> 1, BigInt(2) -> 2)

  def totientChainLength(num: BigInt) = {
    val nextElement = EulerMath.totient(num)
    val result = 1 + chainLengths.getOrElse(nextElement, throw new RuntimeException("@@@@"))
    chainLengths += num -> result
    result
  }

  val totientChains = (BigInt(3) to 32048) map totientChainLength
  totientChains foreach { chain =>
    println(chain)
    //    assert(chain.drop(1).dropRight(1) forall {_ % 2 == 0})
  }

//  println(totientChainLength(5))
  assert(totientChainLength(5) == 4)


  println(b2pow22)
  println(createTotientChainBrute(b2pow22).length)


}
