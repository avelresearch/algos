import org.scalatest.FlatSpec

class ConnectedComponentsSpec extends FlatSpec {
  "Path exists" should "return 0" in {
    val graph : Array[List[Int]] =
      """
        |4 2
        |1 2
        |3 2
      """.stripMargin
        .split("\n")
        .filter(_.nonEmpty)
        .map(_.split(" "))
        .map(a => a.map(_.toInt))
        .map(_.toList)
    val testable = new ConnectedComponents(4, 2, graph)
    val res = testable.run()
    assert(res == 2, "Path must exists")
  }
}
