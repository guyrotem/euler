package euler.problems

import euler.utils.EulerUtils

/**
  * Created on 24/07/2016.
  */
object LaserBeam extends App {

  def calcOptionsPrism(numEdges: BigInt): BigInt = {
    if (numEdges % 2 == 0) return 0

    val level = (numEdges + 3) / 2

    solveLevel(level)
  }

  def numResilient(n: BigInt): BigInt = {
    //  see TOTIENT FUNCTION
    val primeFactors = EulerUtils.fullDecomposeBig(n).keys
    n * (primeFactors map {_ - 1}).product / primeFactors.product
  }

  private def solveLevel(level: BigInt): BigInt = {
    println(s"level: $level, ${level % 3}")
    val factors = EulerUtils.fullDecomposeBig(level).keys
    println(factors)

    val exitsPerLevel = ((level + 1)/3).toInt
    //  exitsPerLevel ~ MAX INT, level ~== 1 mod 3, 3x-(lmod3) € 2 mod 3
    //  divisors: List(5, 5, 11, 17, 23, 29, 41, 47) => all divisors are € 2 mod 3
    //  valid exits are... $exitsCount - $oddNumberOfComponents((8/1) + (8/3) + (8/5) + (8/7))
    // G1: 0, G2: 1, G3: 2
    val cVector = EulerUtils.bigNaturals map {x => 3*x - (level % 3)} take exitsPerLevel

    println(s"Je n'ai pas besoin d'un faisceau laser! $level $exitsPerLevel")

    //  val count = cVector.count(isCoPrime(level, _))
    val count = cVector.count { (divisor: BigInt) =>
      factors forall { factor =>
        divisor % factor != 0
      }
    }
    println("Fertig!")
    count
  }

//  println(s"!! ${EulerUtils.decomposeBig(80840)} @@") => 2 2 2 5 43 47

  assert(calcOptionsPrism(11) == 2, s"Calc prism: ${calcOptionsPrism(11)} != 2")
  assert(calcOptionsPrism(1000001) == 80840, s"Calc prism: ${calcOptionsPrism(1000001)} != 80840")

  //val res = (BigInt(0) to 100) map {107 + 12*_} map calcOptionsPrism
  //println(((BigInt(0) to 100) map {55 + 6*_} map {x=>(numResilient(x), EulerUtils.decomposeBig(x))}) zip res)

  //  ... if level is prime, solution is numResilient / 3. else ???
//  calcOptionsPrism(BigInt("12017639147"))
  //  NOTE: level = BigInt("6008819575")

}
