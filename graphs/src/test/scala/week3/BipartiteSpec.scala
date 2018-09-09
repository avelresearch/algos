import org.scalatest.{FlatSpec, Ignore}
import scala.collection.mutable.ListBuffer

//TODO: enable unit test
@Ignore
class BipartiteSpec extends FlatSpec {
  trait Fixture {
    def toGraph(m: Int, str: String): Array[List[Int]] = {
      val data = str.stripMargin
        .split("\n")
        .map(_.trim())
        .filter(_.nonEmpty)
        .map(_.split(" "))
        .map(a => a.map(_.toInt))

      val g: Array[ListBuffer[Int]] = Array.fill(m)(ListBuffer[Int]())
      data.foreach(p => {
        val (x, y) = (p(0), p(1))
        g(x - 1).append(y - 1)
        g(y - 1).append(x - 1)
      })
      g.map(_.toList)
    }
  }

  "Next graph" should "be not bipartite" in new Fixture {
    val data =
      """
        |1 2
        |4 1
        |2 3
        |3 1
      """.stripMargin
    val g = this.toGraph(4, data)
    val testable = new Bipartite(4, g)
    val res = testable.run()
    assert(res == 0, "This is not bipartite graph")
  }
}
