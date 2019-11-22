package euler.problems

import euler.utils.EulerUtils

/**
  * Created on 25/12/2016.
  */
object E215CrackFreeWalls extends App {
  private def makeAllRows(blockSizes: Seq[Int]): Seq[Seq[Int]] = {
    if (blockSizes.isEmpty) {
      Seq(Seq.empty)
    } else {
      makeAllRows(blockSizes.tail) flatMap { row =>
        (0 to row.length) map { l =>
          row.take(l) ++ blockSizes.take(1) ++ row.takeRight(row.length - l)
        }
      } distinct
    }
  }

  private def sumRow(row: Seq[Int]): Seq[Int] = {
    row.indices map { x =>
      row.take(x).sum
    } drop 1  //  1st is always 0
  }

  def makeAllPatternsFromBlocks(blocks: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    blocks flatMap makeAllRows map sumRow
  }

  def isCrackFreeRows(row1: Seq[Int])(row2: Seq[Int]): Boolean = {
    row1.intersect(row2).isEmpty
  }

  def createCrackFreeGraph(blockPatterns: Seq[Seq[Int]]): Map[Seq[Int], Seq[Seq[Int]]] = {
    blockPatterns map { pattern =>
      pattern -> (blockPatterns filter isCrackFreeRows(pattern))
    } toMap
  }

  def countNLongPaths[NodeId](graph: Map[NodeId, Seq[NodeId]], N: Int): BigInt = {
    def buildNextGraph(currentGraphCount: Map[NodeId, BigInt]): Map[NodeId, BigInt] = {
      currentGraphCount map {
        case (nodeId, _) =>
          val paths = (graph(nodeId) map { neighbor =>
            currentGraphCount(neighbor)
          }).sum
          (nodeId, paths)
      }
    }

    val graph0 = (graph.keys map {(_, BigInt(1))}).toMap

    val graphN = EulerUtils.repeatNTimes(buildNextGraph, graph0, N)
    graphN.values.sum
  }

  def makeAllBlocks(rowWidth: Int): Seq[Seq[Int]] = {
    val max2 = rowWidth / 2 - rowWidth % 2
    val min2 = max2 % 3
    min2.to(max2, 3) map { count2 =>
      val count3 = (rowWidth - 2 * count2) / 3
      Seq.fill(count2)(2) ++ Seq.fill(count3)(3)
    }
  }

  def w(rowWidth: Int, numRows: Int): BigInt = {
    val blocks = makeAllBlocks(rowWidth)
    val patterns = makeAllPatternsFromBlocks(blocks)
    val neighbors = createCrackFreeGraph(patterns)
    val solution = countNLongPaths(neighbors, numRows - 1)

    println("number of patterns: " + patterns.length)
    println("number of edges in graph: " + neighbors.values.map(_.length).sum)

    solution
  }

  println(w(32, 10))

//  number of patterns: 3329
//  number of edges in graph: 37120
//  806844323190414
}
