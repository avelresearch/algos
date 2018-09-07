import org.scalatest.FlatSpec

class AcyclicitySpec extends FlatSpec {

  import SimpleTypeConverter.stringToArray

  "Directed graph" should "be consistent" in {
    val graph: Array[List[Int]] =
      """
        |1 2
        |4 1
        |2 3
        |3 1
      """.stripMargin
    val testable = new Acyclicity(4, 4, graph)
    val res = testable.explorerAll()
    assert(res == 1, "Must be 1")
  }

  // TODO: add test case for Acyclic incosistent graph
  //  "Directed graph" should "be inconsistent" in {
  //    val graph: Array[List[Int]] =
  //      """
  //        |1 2
  //        |2 3
  //        |3 4
  //        |4 2
  //      """.stripMargin
  //    val testable = new Acyclicity(4, 4, graph)
  //    val res = testable.explorerAll()
  //    assert(res == 0, "Must be 0")
  //  }
}
