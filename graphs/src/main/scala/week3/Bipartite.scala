import java.util.Scanner
import scala.collection.mutable.ListBuffer

/*
  Problem: Computing the Minimum Number of Flight Segments
  Problem Introduction
  You would like to compute the minimum number of flight segments to get from one city
  to another one. For this, you construct the following undirected graph:
  vertices represent cities, there is an edge between two vertices whenever there
  is a flight between the corresponding two cities.
  Then, it suffices to find a shortest path from one of the given cities to the other one.
 */

class Bipartite(n: Int, a: Array[List[Int]] ) {
  def run() : Int = -1
}

object Bipartite {

  class InputReader {
    val scanner = new Scanner(System.in)

    def init() = (scanner.nextInt(), scanner.nextInt())

    def getGraph(n: Int, m: Int): Array[ListBuffer[Int]] = {
      val adj: Array[ListBuffer[Int]] = Array.fill(n)(ListBuffer[Int]())
      for (i <- 1 to m) {
        val (x, y) = (scanner.nextInt(), scanner.nextInt())
        adj(x - 1).append(y - 1)
        adj(y - 1).append(x - 1)
      }
      adj
    }
  }

  def main(args: Array[String]): Unit = {
    val reader = new InputReader()
    val (n, m) = reader.init()
    val adj: Array[ListBuffer[Int]] = reader.getGraph(n, m)

    val b = new Bipartite(n, adj.map(_.toList) )
    val res = b.run()
    println(res)
  }

}

