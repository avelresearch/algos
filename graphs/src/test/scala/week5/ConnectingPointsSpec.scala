import org.scalatest.{FlatSpec, Ignore}

class ConnectingPointsSpec extends FlatSpec {

  trait Fixture {
    def toGraph(n: Int, str: String): (Array[Int], Array[Int]) = {
      val data = str.stripMargin
        .split("\n")
        .map(_.trim())
        .filter(_.nonEmpty)
        .map(_.split(" "))
        .map(a => a.map(_.toInt))

      val x: Array[Int] = Array.fill(n)(0)
      val y: Array[Int] = Array.fill(n)(0)

      data.zipWithIndex.foreach(p => {
        val (a, b) = (p._1(0), p._1(1))
        x(p._2) = a
        y(p._2) = b
      })

      (x, y)
    }
  }

  "Minumule legth of segment" should "be 3.0000" in new Fixture {
    val data =
      """
        |0 0
        |0 1
        |1 0
        |1 1
      """.stripMargin
    val (x, y) = this.toGraph(4, data)
    val testable = new ConnectingPoints(4, x, y)
    val res = testable.minimumDistance()
    assert(res == 3.0000, "Minimum total length of segment should be: 3.000000000")
  }

  "Minumule legth of segment" should "be 7.0644" in new Fixture {
    val data =
      """
        |0 0
        |0 2
        |1 1
        |3 0
        |3 2
      """.stripMargin
    val (x, y) = this.toGraph(5, data)
    val testable = new ConnectingPoints(5, x, y)
    val res = testable.minimumDistance()
    assert(res == 7.06449510224598, "Minimum total length of segment should be: 7.0644")
  }
}
