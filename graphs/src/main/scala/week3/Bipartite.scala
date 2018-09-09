import java.util.Scanner
import scala.collection.mutable.ListBuffer

/*
  Problem: Checking whether a Graph is Bipartite
 Problem Introduction
 An undirected graph is called bipartite if its vertices can be split into two parts such that
 each edge of the graph joins to vertices from different parts. Bipartite graphs arise naturally
 in applications where a graph is used to model connections between objects of two different
 types (say, boys and girls; or students and dormitories).
 An alternative definition is the following: a graph is bipartite
 if its vertices can be colored with two colors (say, black and white) such that
 the endpoints of each edge have different colors.
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

