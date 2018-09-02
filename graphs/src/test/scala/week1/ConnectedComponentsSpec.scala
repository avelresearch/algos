import org.scalatest.FlatSpec

class ConnectedComponentsSpec extends FlatSpec {
  import SimpleTypeConverter.stringToArray
  "Path exists" should "return 0" in {
    val graph : Array[List[Int]] =
      """
        |4 2
        |1 2
        |3 2
      """.stripMargin
    val testable = new ConnectedComponents(4, 2, graph)
    val res = testable.run()
    assert(res == 2, "Path must exists")
  }
}
