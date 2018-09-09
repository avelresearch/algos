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

class BFS(n: Int, a: Array[List[Int]] ) {

  def distance( s: Int, t: Int): Int = {
    val dist = Array.fill(n)(Int.MaxValue)
    dist.update(s, 0)

    def distAcc(q: ListBuffer[Int]): Unit = q match {
      case a if (a.isEmpty) => Unit
      case _ => {
        val u = q.head
        q -= u
        a(u).foreach(v => {
          if (dist(v) == Int.MaxValue) {
            q += v
            dist.update(v, dist(u) + 1)
          }
        });
        distAcc(q)
      }
    }

    val queue: ListBuffer[Int] = ListBuffer(s)
    distAcc(queue)

    if (dist(t) == Int.MaxValue) -1 else dist(t)
  }
}

object BFS {

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
    val (s, t) = (reader.scanner.nextInt() - 1, reader.scanner.nextInt() - 1)
    val b = new BFS(n, adj.map(_.toList) )
    val res = b.distance(s, t)
    println(res)
  }

}
