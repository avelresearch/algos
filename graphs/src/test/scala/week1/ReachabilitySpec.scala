import org.scalatest.FlatSpec

class ReachabilitySpec extends FlatSpec {
  import SimpleTypeConverter.stringToArray

  "Path from 1 to 4" should "exists" in {
    val graph: Array[List[Int]] =
      """
        |1 2
        |3 2
        |4 3
        |1 4
      """.stripMargin
    val testable = new Reachability(4, 4, graph, 1, 4)
    val res = testable.explorer(1)
    assert(res == 1, "Path must exists")
  }


  "Path from 1 to 4" should "not exists" in {
    val graph: Array[List[Int]] =
      """
        |1 2
        |3 2
      """.stripMargin
    val testable = new Reachability(4, 2, graph, 1, 4)
    val res = testable.explorer(1)
    assert(res == 0, "Path must NOT exist")
  }


}
