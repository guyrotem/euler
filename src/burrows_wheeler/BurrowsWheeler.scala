package burrows_wheeler

/**
  * Created on 30/07/2016.
  */
object BurrowsWheeler {
  def bwMatch(bwTransform: String, pattern: String): Seq[Int] = {
    val firstColumn = bwTransform.sorted
    val reversePattern = pattern.reverse

    def bwMatchIteration(lastIteration: (Int, Seq[Int])): (Int, Seq[Int]) = {
      lastIteration match {
        case (index: Int, prevIndices: Seq[Int]) =>
          val nextChar = reversePattern(index + 1)
          val survivors = prevIndices filter {
            bwTransform(_) == nextChar
          }
          //  next indices are consecutive. no need to find it for every index...
          val nextIndices = survivors map lastToFirst
          (index + 1, nextIndices)
      }
    }

    def lastToFirst(lastColumnIndex: Int): Int = {
      val currentChar = bwTransform(lastColumnIndex)
      val n = bwTransform.substring(0, lastColumnIndex + 1).count({_ == currentChar})
      firstColumn.indexOf(currentChar) + n - 1
    }

    val decoderHead = (0, firstColumn.indexOf(reversePattern.head) to firstColumn.lastIndexOf(reversePattern.head))

    lazy val decodingStream: Stream[(Int, Seq[Int])] = decoderHead #:: (decodingStream map bwMatchIteration)

    decodingStream(pattern.length - 1)._2
  }

  def bwtNaive(input: String): String = {
    val terminatedInput = input + "$"
    val cyclicSquareMatrix = (0 until terminatedInput.length) map rotateTextAt(terminatedInput)
    val burrowsWheelerRow = cyclicSquareMatrix.sorted map {_.last}
    joinSeq(burrowsWheelerRow)
  }

  def inverseBwt(input: String): String = {
    val lastColumn = input
    val firstColumn = lastColumn.sorted

    val firstIndexToFind = lastColumn.indexOf("$")

    def fastBwtDecodeIteration(currentIndex: Int): Int = {
      val nextCharToFind = lastColumn(currentIndex)
      val charAppearance = lastColumn.substring(0, currentIndex).count({_ == nextCharToFind}) + 1
      val nextIndexToFind = findNthAppearanceOf(firstColumn, charAppearance, nextCharToFind)
      nextIndexToFind
    }

    lazy val decodedBwtIndices = transformNTimes(firstIndexToFind, fastBwtDecodeIteration, input.length - 1)

    val decodedBwt = decodedBwtIndices.tail map {lastColumn(_)}

    decodedBwt.reverse.foldRight("") {_ + _}
  }

  def inverseBwtNaive(input: String) = {
    def bwtDecodeIteration(current: Seq[String], sorted: Seq[String]) = {
      ((sorted zip current) map { case (pre, str) => pre + str }).sorted
    }

    val encodedAsCharArray = input.split("")

    lazy val bwtStream =
      transformNTimes(Seq.fill(input.length)(""), {x: Seq[String] => bwtDecodeIteration(x, encodedAsCharArray)}, input.length)

    bwtStream(input.length).head.substring(1)
  }

  private def transformNTimes[T](zeroElement: T, transformer: T => T, times: Int) = {
    lazy val transformationStream: Stream[T] = zeroElement #:: (transformationStream map transformer)
    transformationStream take(times + 1) toList
  }

  private def joinSeq(chars: Seq[Char]): String = {
    chars.foldLeft("") {_ + _}
  }

  private def findNthAppearanceOf(sorted: String, n: Int, char: Char): Int = {
    require(sorted.sorted == sorted)
    sorted.indexOf(char) + n - 1
  }

  private def rotateTextAt(text: String)(index: Int): String = text.substring(index) + text.substring(0, index)

}
