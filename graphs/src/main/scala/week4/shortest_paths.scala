import java.util.Scanner
import ShortestPathsTypes.Edges
import scala.collection.mutable.ListBuffer

/*
  Advanced Problem: Exchanging Money Optimally
  Problem Introduction
    Now, you would like to compute an optimal way of exchanging
    the given currency ci into all other currencies. For this, you
    find shortest paths from the vertex ci to all the other vertices.
 */

object ShortestPathsTypes {
  type Edges = Array[List[Int]]
}

// TODO: implement algorithm
class ShortestPaths(n: Int, m:Int, adj: Edges, cost: Edges){

  def run(): Int = -1
}

object shortest_paths {

    class InputReader {
      val scanner = new Scanner(System.in);

      def init() = (scanner.nextInt(), scanner.nextInt())

      def getGraph(n: Int, m: Int): (Edges, Edges) = {
        val adj = Array.fill(n)(ListBuffer[Int]())
        val cost = Array.fill(n)(ListBuffer[Int]())
        for (i <- 1 to m) {
          val (x, y, w) = (scanner.nextInt(), scanner.nextInt(), scanner.nextInt())
          adj(x - 1).append(y - 1)
          cost(x - 1).append(w)
        }
        (adj.map(_.toList), cost.map(_.toList))
      }
    }

    def main(args: Array[String]): Unit = {
      val reader = new InputReader()
      val (n: Int, m: Int) = reader.init()
      val (adj, cost) = reader.getGraph(n, m)
      val neg = new ShortestPaths(n, m, adj, cost)
      println(neg.run())
    }

  }