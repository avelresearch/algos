import org.scalatest.FlatSpec

import scala.collection.mutable.ListBuffer

/*
4 4
1 2 -5
4 1 2
2 3 2
3 1 1
 */
class NegativeCycleSpec extends FlatSpec {
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
        |1 2 -5
        |4 1 2
        |2 3 2
        |3 1 1
      """.stripMargin
    val (adj, cost) = this.toGraph(4, data)
    val testable = new NegativeCycle(4, 4, adj, cost)
    val res = testable.run()
    assert(res == 1, "Distance should be 6")
  }
}
