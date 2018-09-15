import Clustering.{ Edges, Point}
import scala.io.StdIn

class DisjointSet(n : Int) {
  val parent: Array[Int] = Array.ofDim[Int](n + 1)
  val rank: Array[Int] = Array.ofDim[Int](n + 1)
  var pathCompression: Boolean = true
  var clusters: Int = n

  for (i <- parent.indices.tail) makeSet(i)

  def makeSet(i: Int): Unit = {
    parent(i) = i
    rank(i) = 0
  }

  def find(i: Int): Int = {
    if (pathCompression) {
      if (i != parent(i)) {
        parent(i) = find(parent(i))
      }
      parent(i)
    }
    else {
      var k = i
      while (k != parent(k)) {
        k = parent(k)
      }
      k
    }
  }

  def union(i: Int, j: Int): Unit = {
    val iRoot = find(i)
    val jRoot = find(j)
    if (iRoot == jRoot) return
    if (rank(iRoot) > rank(jRoot)) {
      parent(jRoot) = iRoot
    }
    else {
      parent(iRoot) = jRoot
      if (rank(iRoot) == rank(jRoot)) {
        rank(jRoot) += 1
      }
    }
    clusters -= 1
  }
}

class Clustering(edges: Edges){
  def compute(k : Int) : Double = {
    val indexedVertices = edges.keys.toIndexedSeq
    val pointToIndexMap = indexedVertices.zipWithIndex.toMap
    val pointsSets = new DisjointSet(indexedVertices.size)
    indexedVertices.zipWithIndex.foreach { case (_, i) => pointsSets.makeSet(i) }

    val sortedPairs = indexedVertices.flatMap { k => edges(k).map((k, _)) }.sortBy {
      case (p, q) => Math.abs(p - q)
    }

    var minimumSpanningTree : Set[(Point, Point)] = Set.empty
    for ((p, q) <- sortedPairs) {
      val clusters = pointsSets.clusters
      if (clusters != k - 1) {
        val pIndex = pointToIndexMap(p)
        val qIndex = pointToIndexMap(q)
        if (pointsSets.find(pIndex) != pointsSets.find(qIndex)) {
          minimumSpanningTree = minimumSpanningTree + Tuple2(p, q)
          pointsSets.union(pIndex, qIndex)
        }
      }
      else {
        return minimumSpanningTree.map { case (p, q) => Math.abs(p - q) }.max
      }
    }

    Double.PositiveInfinity
  }
}

object Clustering {
  type Edges = Map[Point, Set[Point]]

  case class Point(x : Int, y  : Int) {
    def -(that: Point) : Double = Math.sqrt(Math.pow(x - that.x, 2) + Math.pow(y - that.y, 2))
  }

  def main(args: Array[String]): Unit = {
    val n = StdIn.readInt
    var allPoints = Set.empty[Point]

    for (_ <- 1 to n) {
      val line = readLineAsInts()
      allPoints = allPoints + Point(line.head, line(1))
    }
    val k = StdIn.readInt
    val edges = allPoints.map(p => (p, allPoints)).toMap
    val c = new Clustering(edges)
    println(c.compute( k))
  }

  def readLineAsInts() : Seq[Int] = StdIn.readLine.trim.split(" ").map(_.toInt)

}