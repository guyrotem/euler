package euler.utils

import FractionImplicits.`Int --> Fraction`

/**
  * Created on 22/07/2016.
  */
object DiophantineTools {
  def rootAsContinuedFraction(n: Int): (Int, Seq[Int]) = {
    val whole = Math.sqrt(n).floor.toInt
    val firstSegment = DiophantineSegment(n, whole, n - whole * whole)

    val diopStream: Stream[DiophantineSegment] = makeDiopStream(firstSegment)
    val period =
      if (firstSegment.denom == 0) 0
      else diopStream.tail.indexOf(firstSegment) + 1

    (whole, diopStream.take(period).toList map {_.whole})
  }

  def valueOfPartialFractionSequence(sequence: Seq[Int]): Fraction = {
    sequence.foldRight(0.asFraction)({ (current: Int, acc: Fraction) =>
      (acc + current.asFraction).invert
    })
  }

  def calculateDiophantineSequenceValue(sequence: Seq[Int], length: Int): Fraction = {
    assert(sequence.nonEmpty)
    valueOfPartialFractionSequence(createCyclicStream(sequence) take length)
  }

  private def createCyclicStream[T](sequence: Seq[T]): Stream[T] = {
    sequence.toStream #::: createCyclicStream(sequence)
  }

  private def makeDiopStream(start: DiophantineSegment): Stream[DiophantineSegment] =
    start #:: makeDiopStream(start.nextSegment)

  case class DiophantineSegment(root: Int, nom: Int, denom: Int) {
    def whole = ((Math.sqrt(root) + nom) / denom).floor.toInt
    def nextSegment: DiophantineSegment = {
      val nomFraction = nom - denom * whole
      DiophantineSegment(root, -nomFraction, (root - nomFraction * nomFraction) / denom)
    }
  }
}

