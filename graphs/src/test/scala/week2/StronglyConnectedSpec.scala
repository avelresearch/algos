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

}
