package euler.problems

import euler.utils.EulerMath
import euler.utils.EulerMath.BigFraction

import scala.collection.mutable

import scala.language.postfixOps

object E155Capacitors {

  val INITIAL_CAPACITY = EulerMath.CreateFraction(60, 1)
  val knownSolutions: scala.collection.mutable.Map[Int, Set[BigFraction]] = mutable.Map(1 -> Set(INITIAL_CAPACITY))

  private def parallel(c1: BigFraction, c2: BigFraction): BigFraction = {
    c1 + c2
  }

  private def serial(c1: BigFraction, c2: BigFraction): BigFraction = {
    (c1 * c2) / (c1 + c2)
  }

  def calcNumCapacitors(numCapacitors: Int): Set[BigFraction] = {
    println(s"Starting level $numCapacitors ${knownSolutions(numCapacitors-1).size}")
    (1 to numCapacitors/2) flatMap { split: Int =>
      val sideA = knownSolutions.getOrElse(split, Set.empty)
      val sideB = knownSolutions.getOrElse(numCapacitors - split, Set.empty)

      sideA flatMap {c1 =>
        sideB flatMap {c2 =>
          Seq(parallel(c1, c2), serial(c1, c2))
        }
      }
    } toSet
  }

  val LEVELS = 18

  (2 to LEVELS) foreach { numCapacitors =>
    knownSolutions += numCapacitors -> calcNumCapacitors(numCapacitors)
  }

  println("DONE!")

  (1 to LEVELS) foreach { n =>
    println(knownSolutions(n).size)
  }

  println("Solution: " + (knownSolutions flatMap {_._2}).toSet.size)

  //  for 17: 1529533
  //  for 18: 3857447
  println((knownSolutions map {_._2.size}).sum)

}
