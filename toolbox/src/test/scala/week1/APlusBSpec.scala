import org.scalatest.FlatSpec

class APlusBSpec extends FlatSpec {
  "APlusB" should "return 5 arguments 2 and 3" in {
    val input = "2 3"
    val testable = new APlusB(input)
    assert(testable.run() === 5)   
  } 
}
