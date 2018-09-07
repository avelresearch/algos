import java.util.Scanner
import scala.collection.mutable.ListBuffer

class StronglyConnected(n : Int, m: Int, vertexes: Array[List[Int]]) {
  def run() : Int = ???
}

object StronglyConnected {
  class InputReader{
    val scanner = new Scanner(System.in)
    def init() = (scanner.nextInt(), scanner.nextInt() )
    def getGraph(n:Int, m:Int ): Array[List[Int]] = {
      val vertexes: Array[ListBuffer[Int]] = Array.fill(n)(ListBuffer[Int]())
      (1 to m).foreach(_ => {
        val (x, y) = (scanner.nextInt(), scanner.nextInt())
        vertexes(x - 1).append( y )
      })
      vertexes.map(_.toList)
    }
  }
  def main(args: Array[String]): Unit = {
    val reader = new InputReader()
    val (n,m) = reader.init()
    val g = reader.getGraph(n, m)
    val scc = new StronglyConnected(n, m, g )
    val result = scc.run()
    println(result)
  }
}
