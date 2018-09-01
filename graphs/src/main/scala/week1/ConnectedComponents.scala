import java.util.Scanner
import scala.collection.mutable.ListBuffer

object ConnectedComponents
{

  def main(args: Array[String]): Unit = {
    var res = 1
    val scanner = new Scanner(System.in);
    val (n, m) = (scanner.nextInt(), scanner.nextInt() )
    val vertexes : Array[ ListBuffer[Int] ] = Array.fill(n)( ListBuffer[Int]() )
    val visited : Array[Int] = new Array[Int](n)

    (1 to m).foreach(_ => {
      val x = scanner.nextInt()
      val y = scanner.nextInt()
      vertexes(x - 1).append( y )
      vertexes(y - 1).append( x )
    })

    def explore(v : Int) : Boolean = {
      def exlporeAcc(a: List[Int]) : Boolean = a match {
        case List() => true
        case h::tail =>
          if ( visited(h - 1) == 0) explore( h )
          exlporeAcc(tail)
      }
      visited.update(v - 1, res)
      val n = vertexes(v - 1).filter(x => visited(x - 1) == 0)
      if (n.length > 0) exlporeAcc(n.toList)
      else false
    }

    var ready : Boolean = false
    var vertex : Int = 1

    while(ready == false){
      explore(vertex)
      ready = visited.forall(x => x > 0)
      if (!ready) {
        vertex = visited
          .zipWithIndex
          .filter(x => x._1 == 0)(0)._2 + 1
        res = res + 1
      }
    }
    println(res)
  }

}