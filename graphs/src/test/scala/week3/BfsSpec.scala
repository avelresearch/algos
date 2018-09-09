import org.scalatest.FlatSpec
import scala.collection.mutable.ListBuffer

class BfsSpec extends FlatSpec {

  trait Fixture {
    def toGraph(m: Int, str: String) : Array[List[Int]] = {
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


  "Distance between 2 and 4" should "be 2" in new Fixture {

    val data =
      """
        |1 2
        |4 1
        |2 3
        |3 1
      """.stripMargin
    val g = this.toGraph(4, data)

    val testable = new BFS(4, g)
    val res = testable.distance(1, 3)
    assert(res == 2, "Number of edges in the path")
  }

}