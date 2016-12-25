package euler.tests

/**
  * Created on 24/12/2016.
  */
object E169SumPowersOfTwo extends App {
  type TrinaryBinary = Seq[Int]

  def fBrute(n: Int): Int = {
    val maxBase2 = Math.floor(Math.log(n) / Math.log(2)).toInt

    getAllSums(maxBase2) count {_ == n}
  }

  def f(n: BigInt): BigInt = {
    solveSumPower(n)
  }

  @Deprecated
  private def makeAMove(binaryRep: TrinaryBinary): Seq[TrinaryBinary] = {
    (1 until binaryRep.length) flatMap { x =>
      if (binaryRep(x - 1) == 0 && binaryRep(x) > 0) {
        Seq(
          binaryRep.zipWithIndex map {
            case(v, idx) =>
              if (idx == x - 1)
                v + 2
              else if (idx == x)
                v - 1
              else
                v
          }
        )
      } else {
        Seq.empty
      }
    }
  }

  private def binaryDecomposition(n: BigInt): TrinaryBinary = {
    def help(x: BigInt, acc: Seq[Int]): Seq[Int] = {
      if (x == 0)
        acc
      else
        help(x / 2, acc ++ Seq((x%2).toInt))
    }

    help(n, Seq.empty)
  }

  private def compressTriBi(x: TrinaryBinary): Seq[Int] = {
    def help(y: TrinaryBinary, acc: Seq[Int]): Seq[Int] = {
      if (y.isEmpty)
        acc
      else {
        val idx = y.indexOf(1)
        help(y.drop(idx+1), acc :+ idx)
      }
    }
    help(x, Seq.empty)
  }

  private def getAllSums(maxIndex: Int): Seq[BigInt] = {
    if (maxIndex == -1) {
      Seq(0)
    } else {
      val nMinusOne = getAllSums(maxIndex - 1)
      Seq(0, 1, 2) map {
        _ * Math.pow(2, maxIndex).toInt
      } flatMap { x =>
        nMinusOne map {x + _}
      }
    }
  }

  def solveSumPower(x: BigInt): BigInt = {
    val binaryDecomp = binaryDecomposition(x)
    val compressed = compressTriBi(binaryDecomp)

    def calcNextPhase(prevPrev: BigInt, prev: BigInt, nextInput: Int): BigInt = {
      prevPrev * (nextInput + 1) + (prev - prevPrev) * (nextInput + 2)
    }

    lazy val solutionBase: Stream[BigInt] =
      BigInt(1) #::
        Stream(BigInt(compressed.head) + 1) #:::
        (((solutionBase zip solutionBase.tail) zip compressed.tail) map {
          case ((prevPrev, prev), nextZerosLength) => calcNextPhase(prevPrev, prev, nextZerosLength)
        })

    solutionBase.last
  }


  assert(fBrute(10) == 5, s"${fBrute(10)} != 5")  //  1010, 1002, 0210, 0202, 0122

  assert(binaryDecomposition(15) == Seq(1, 1, 1, 1))
  assert(binaryDecomposition(32) == Seq(0, 0, 0, 0, 0, 1))
  //List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1)
  //  f(2^n) == n+1
  //  f(14405) == 138

  //  0[n]1 => n+1
  //  0[n]10[m]1  => (n+1)(m+2)-1
  //  0[n]10[m]10[l]1  => ?
  //  1   |  n
  //  m+1 | m+2
  //
  //  n+1 | (n+1)(m+1)-1
  //  l+1 | l+2
  //  (n+1)(l+1) + (n+1)(m+1)(l+2) - (l+2)

  //{
  //  s[p] = s[p-2] * (x_p+1) + (s[p-1] - s[p-2]) * (x_p+2)
  //  s[0] = 1
  //  s[1](x_1) = x_1+1
  //}

  //  ... s[19](25, 1, 2, 4, 2, 1, 9, 1, 4, 0, 1, 3, 1, 1, 2, 0, 1, 3, 4) = ???

  val tenPow25 = BigInt("10000000000000000000000000")
  println(binaryDecomposition(tenPow25))  //  84 bits
  println(solveSumPower(tenPow25))  //178653872807
}

