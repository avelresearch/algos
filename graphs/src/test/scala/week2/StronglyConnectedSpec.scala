import org.scalatest.FlatSpec

class StronglyConnectedSpec extends FlatSpec {

  import SimpleTypeConverter.stringToArray

  "Number of scc" should "be 2" in {
    val graph: Array[List[Int]] =
      """
        |1 2
        |4 1
        |2 3
        |3 1
      """.stripMargin
    val testable = new StronglyConnected(4, 4, graph)
    val res = testable.run()
    assert(res == 2, "must be 2")
  }

  "Number of scc" should "be 2" in {
    val graph: Array[List[Int]] =
      """
        |2 1
        |3 2
        |3 1
        |4 3
        |4 1
        |5 2
        |5 3
      """.stripMargin
    val testable = new StronglyConnected(5, 7, graph)
    val res = testable.run()
    assert(res == 5, "must be 2")
  }

}
