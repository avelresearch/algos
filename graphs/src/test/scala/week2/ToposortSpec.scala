import org.scalatest.FlatSpec
import scala.collection.mutable.ListBuffer

class ToposortSpec extends FlatSpec {
  "Directed graph" should "be consistent" in {
    val _adj =
      """
        |2 1
        |3 2
        |3 1
        |4 3
        |4 1
        |5 2
        |5 3
      """.stripMargin
        .split("\n")
        .map(_.trim())
        .filter(_.nonEmpty)
        .map(_.split(" "))
        .map(a => a.map(_.toInt))

    val adj: Array[ListBuffer[Int]] = Array.fill(5)(ListBuffer[Int]())
    _adj.foreach(p => {
      val (x, y) = (p(0), p(1))
      adj(x - 1).append(y - 1)
    })

    val testable = new Toposort(5, 7, adj.map(_.toList))
    testable.run()
    assert(testable.order.reverse == List(5, 4, 3, 2, 1), "Must be 5, 4, 3, 2, 1")
  }

}
