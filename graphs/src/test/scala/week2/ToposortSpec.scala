import org.scalatest.FlatSpec
import scala.collection.mutable.ListBuffer

class ToposortSpec extends FlatSpec {

  trait Fixture {
    def toGraph(str: String) : Array[List[Int]] = {
      // String to Array
      val data = str.split("\n")
        .map(_.trim())
        .filter(_.nonEmpty)
        .map(_.split(" "))
        .map(a => a.map(_.toInt))

      // Array to Array[List[Int]]
      val adj: Array[ListBuffer[Int]] = Array.fill(5)(ListBuffer[Int]())
      data.foreach(p => {
        val (x, y) = (p(0), p(1))
        adj(x - 1).append(y - 1)
      })
      adj.map(_.toList)
    }
  }

  "Directed graph" should "have next topoligical order 5, 4, 3, 2, 1" in new Fixture {
    val str =
      """
        |2 1
        |3 2
        |3 1
        |4 3
        |4 1
        |5 2
        |5 3
      """.stripMargin
    val graph = toGraph(str)

    val testable = new Toposort(5, 7, graph)
    testable.run()
    assert(testable.order.reverse == List(5, 4, 3, 2, 1), "Must be 5, 4, 3, 2, 1")
  }

}
