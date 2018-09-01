import org.scalatest.FlatSpec

class ConnectedComponentsSpec extends FlatSpec {
  "Path exists" should "return 0" in {
    val input =
      """
        |4 2
        |1 2
        |3 2
      """.stripMargin
    val graph: Array[List[Int]] = input.split("\n")
      .filter(_.nonEmpty)
      .map(_.split(" "))
      .map(a => a.map(_.toInt))
      .map(_.toList)

    val cc = new ConnectedComponents(4, 2, graph)
    val res = cc.run()
    assert(res == 2, "Path must exists")
  }
}
