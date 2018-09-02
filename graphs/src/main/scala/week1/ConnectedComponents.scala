import java.util.Scanner
import scala.collection.mutable.ListBuffer

/*
  Problem: Adding Exits to a Maze
  Problem Introduction
  Now you decide to make sure that there are no dead zones in a maze, that is,
  that at least one exit is reachable from each cell.
  For this, you find connected components of the corresponding undirected graph and ensure
  that each component contains an exit cell.
 */
class ConnectedComponents(n : Int, m: Int, vertexes : Array[ List[Int] ]) {

  var res = 1
  val visited : Array[Int] = new Array[Int](n)

  private def explore(v : Int) : Boolean = {
    def exlporeAcc(a: List[Int]) : Boolean = a match {
      case List() => true
      case h::tail =>
        if ( visited(h - 1) == 0) explore( h )
        exlporeAcc(tail)
    }
    visited.update(v - 1, res)
    val n = vertexes(v - 1).filter(x => visited(x - 1) == 0)
    if (n.length > 0) exlporeAcc(n) else false
  }

  def run(): Int  = {
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
    res
  }

}

object ConnectedComponents
{
  def main(args: Array[String]): Unit = {
    val scanner = new Scanner(System.in);
    val (n, m) = (scanner.nextInt(), scanner.nextInt() )
    val vertexes : Array[ ListBuffer[Int] ] = Array.fill(n)( ListBuffer[Int]() )
    (1 to m).foreach(_ => {
      val x = scanner.nextInt()
      val y = scanner.nextInt()
      vertexes(x - 1).append( y )
      vertexes(y - 1).append( x )
    })
    val cc = new ConnectedComponents(n, m, vertexes.map(_.toList))
    val res = cc.run()
    println(res)
  }

}