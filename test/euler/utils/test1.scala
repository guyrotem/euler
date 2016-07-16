package euler.utils

/**
  * Created on 15/07/2016.
  */
class test1 {
  Utils.decompose(998899)

  def allDivisions(parts: Seq[Int]): Seq[(Int, Int)] = {
    def divide(ix: Int, amount: Int): (Int, Int) = {
      val group1 = (0 until amount) filter { elmIx =>
        (ix & (1 << elmIx)) == 0
      } map parts.apply
      val group2 = (0 until amount) filter { elmIx =>
        (ix & (1 << elmIx)) != 0
      } map parts.apply
      (group1.product, group2.product)
    }


    val amount = parts.length
    for (ix <- 0 until Math.pow(2, amount - 1).toInt)
      yield divide(ix, amount)
  }

  def isPalindrome(num: Int): Boolean = {
    num.toString == num.toString.reverse
  }

  def has3dig(tuples: Seq[(Int, Int)]): Boolean = {
    tuples exists { x: (Int, Int) =>
      x._1.toString.length == 3 &&
        x._2.toString.length == 3
    }
  }

  def primesSieve(stream: Stream[Int]): Stream[Int] = {
    stream.head #:: primesSieve(stream.tail filter {
      _ % stream.head != 0
    })
  }

  //primesSieve(naturals2)(1001 - 1)

  def triangular(n: Int): Int = {
    n * (n + 1) / 2
  }

  def numDivisors(num: Int): Int = {
    //  More efficient than checking all numbers, apparently
    val decomp = Utils.fullDecompose(num)
    decomp.map(_._2 + 1).product
  }

  def allDivisors(num: Int): Set[Int] = {
    ((1 to Math.sqrt(num).toInt) flatMap { i =>
      if (num % i == 0) Seq(i, num / i) else Seq.empty
    }).toSet
  }

  def naturals0: Stream[Int] = 0 #:: (naturals0 map {
    _ + 1
  })

  def subsetSum(target: Int, arr: Seq[Int]): Int = {
    if (target == 0) 1
    else if (arr.isEmpty) 0
    else {
      val useFromCurrentDenomination = 0 to (target / arr.head)
      (useFromCurrentDenomination map {
        x => subsetSum(target - x * arr.head, arr.tail)
      }).sum
    }
  }
}

object test1 extends App {
  val test = new test1

  val pal = for {pOpt <- (900000 to 999999).reverse
       if test.isPalindrome(pOpt) && test.has3dig(test.allDivisions(Utils.decompose(pOpt)))}
    yield pOpt

  println(pal)

  val naturals2: Stream[Int] = 2 #:: (naturals2 map {
    _ + 1
  })
}
