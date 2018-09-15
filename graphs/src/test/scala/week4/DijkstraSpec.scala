import org.scalatest.{FlatSpec, Ignore}
import scala.collection.mutable.ListBuffer

class DijkstraSpec extends FlatSpec {
  type Edges = Array[List[Int]]

  trait Fixture {
    def toGraph(n: Int, str: String): (Edges, Edges) = {
      val data = str.stripMargin
        .split("\n")
        .map(_.trim())
        .filter(_.nonEmpty)
        .map(_.split(" "))
        .map(a => a.map(_.toInt))

      val cost: Array[ListBuffer[Int]] = Array.fill(n)(ListBuffer[Int]())
      val adj: Array[ListBuffer[Int]] = Array.fill(n)(ListBuffer[Int]())

      data.foreach(p => {
        val (x, y, z) = (p(0), p(1), p(2))
        adj(x - 1).append(y - 1)
        cost(x - 1).append(z)
      })
      (adj.map(_.toList), cost.map(_.toList))
    }
  }

  "Next graph" should "be not bipartite" in new Fixture {
    val data =
      """
        |1 2 1
        |4 1 2
        |2 3 2
        |1 3 5
      """.stripMargin
    val (s, t)= (0, 2)
    val (adj, cost) = this.toGraph(4, data)
    val testable = new Dijkstra(4, adj, cost)
    val res = testable.distance(s, t)
    assert(res == 3, "Distance should be 3")
  }
}
