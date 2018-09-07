import java.util.Scanner
import scala.collection.mutable.ListBuffer

/*
  Advanced Problem: Checking Whether Any Intersection in a City is Reachable from Any Other
  Problem Introduction
  The police department of a city has made all streets one-way.
  You would like to check whether it is still possible to drive legally from any intersection
  to any other intersection. For this, you construct a directed graph: vertices are intersections,
  there is an edge (𝑢, 𝑣) whenever there is a (one-way) street from 𝑢 to 𝑣 in the city.
  Then, it suffices to check whether all the vertices in the graph lie in the same
  strongly connected component.
 */
class StronglyConnected(n : Int, m: Int, vertexes: Array[List[Int]]) {
  def run() : Int = ???
}

object StronglyConnected {
  class InputReader{
    val scanner = new Scanner(System.in)
    def init() = (scanner.nextInt(), scanner.nextInt() )
    def getGraph(n:Int, m:Int ): Array[List[Int]] = {
      val vertexes: Array[ListBuffer[Int]] = Array.fill(n)(ListBuffer[Int]())
      (1 to m).foreach(_ => {
        val (x, y) = (scanner.nextInt(), scanner.nextInt())
        vertexes(x - 1).append( y )
      })
      vertexes.map(_.toList)
    }
  }
  def main(args: Array[String]): Unit = {
    val reader = new InputReader()
    val (n,m) = reader.init()
    val g = reader.getGraph(n, m)
    val scc = new StronglyConnected(n, m, g )
    val result = scc.run()
    println(result)
  }
}
