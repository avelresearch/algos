import Clustering.{ Edges, Point}
import scala.io.StdIn

class Clustering(edges: Edges){

  class DisjointSet(n : Int) {
    val parent: Array[Int] = Array.ofDim[Int](n + 1)
    val rank: Array[Int] = Array.ofDim[Int](n + 1)
    var clusters: Int = n

    for (i <- parent.indices.tail) makeSet(i)

    def makeSet(i: Int): Unit = {
      parent(i) = i
      rank(i) = 0
    }

    def find(i: Int): Int = {
      if (i != parent(i)) {
        parent(i) = find(parent(i))
      }
      parent(i)
    }

    def union(i: Int, j: Int): Unit = {
      val aRoot = find(i)
      val bRoot = find(j)
      if (aRoot == bRoot) return
      if (rank(aRoot) > rank(bRoot))
        parent(bRoot) = aRoot
      else {
        parent(aRoot) = bRoot
        if (rank(aRoot) == rank(bRoot)) {
          rank(bRoot) += 1
        }
      }
      clusters -= 1
    }
  }

  def run(k : Int) : Double = {
    val vertices = edges.keys.toIndexedSeq
    val pointsMaps = vertices.zipWithIndex.toMap
    val pointsSets = new DisjointSet(vertices.size)
    vertices.zipWithIndex.foreach { case (_, i) => pointsSets.makeSet(i) }

    val sortedPairs = vertices.flatMap { k => edges(k).map((k, _)) }.sortBy {  case (p, q) => Math.abs(p - q)}

    var mst : Set[(Point, Point)] = Set.empty
    for ((p, q) <- sortedPairs) {
      if (pointsSets.clusters != k - 1) {
        val pIndex = pointsMaps(p)
        val qIndex = pointsMaps(q)
        if (pointsSets.find(pIndex) != pointsSets.find(qIndex)) {
          mst = mst + Tuple2(p, q)
          pointsSets.union(pIndex, qIndex)
        }
      }
      else return mst.map { case (p, q) => Math.abs(p - q) }.max
    }
    // if not distance found
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
      val line = readAsInts()
      allPoints = allPoints + Point(line.head, line(1))
    }
    val k = StdIn.readInt
    val edges = allPoints.map(p => (p, allPoints)).toMap
    val c = new Clustering(edges)
    println(c.run( k))
  }

  def readAsInts() : Seq[Int] = StdIn.readLine.trim.split(" ").map(_.toInt)

}
