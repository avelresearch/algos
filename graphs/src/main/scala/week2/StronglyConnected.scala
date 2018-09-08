import java.util.Scanner
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/*
  Advanced Problem: Checking Whether Any Intersection in a City is Reachable from Any Other
  Problem Introduction
  The police department of a city has made all streets one-way.
  You would like to check whether it is still possible to drive legally from any intersection
  to any other intersection. For this, you construct a directed graph: vertices are intersections,
  there is an edge (𝑢, 𝑣) whenever there is a (one-way) street from 𝑢 to 𝑣 in the city.
  Then, it suffices to check whether all the vertices in the graph lie in the same
  strongly connected component.
 */

case class Vertex(pos: Int, outEdges: Array[Int]){
  var index : Option[Int] = None
  var lowlink : Int = 0
  var onStack : Boolean = false
  def successors(gv: Array[Vertex]) : Array[Vertex] = outEdges.map(i => gv(i) )
}

case class Edge(from: Int, to: Int)

class StronglyConnected(n : Int, m: Int, gv: Array[Vertex]) {
  private var index : Int = 0
  private val result = new ListBuffer[ListBuffer[Vertex]]()
  private val S   = mutable.Stack[Vertex]()

  private def stronglyConnected(v: Vertex) : Unit = {
    v.index = Some(index)
    v.lowlink = index
    this.index = this.index + 1
    S.push(v)
    v.onStack = true

    // Successors of v
    v.successors(gv).foreach(w => w.index match {
      case None =>
        stronglyConnected(w)
        v.lowlink = Math.min(v.lowlink, w.lowlink)
      case _ if (w.onStack) => v.lowlink = Math.min(v.lowlink, w.lowlink)
      case _ =>
    })
    if (v.index != None && v.index.get == v.lowlink){
      val _scc = new ListBuffer[Vertex]()
      result.append(_scc)
      var w : Vertex = null
      do {
        w  = S.pop
        w.onStack = false
        _scc.append(w)
      } while (w.pos != v.pos)
      //println( _scc.map(_.pos).mkString(" ") )
    }
  }

  def run() : Int = {
    gv.foreach(v => v.index match {
      case None => stronglyConnected(v)
      case _ =>
    })
    result.map(_.toList).length
  }

}

object StronglyConnected {

  class InputReader{
    val scanner = new Scanner(System.in)
    def init() = (scanner.nextInt(), scanner.nextInt() )
    def getGraph(n:Int, m:Int ): Array[List[Int]] = {
      val vertexes: Array[ListBuffer[Int]] = Array.fill(n)(ListBuffer[Int]())
      (1 to m).foreach(_ => {
        val (x, y) = (scanner.nextInt(), scanner.nextInt())
        vertexes(x - 1).append( y - 1 )
      })
      vertexes.map(_.toList)
    }
  }

  def main(args: Array[String]): Unit = {
    val reader = new InputReader()
    val (n,m) = reader.init()
    val g = reader.getGraph(n, m)
    val gv : Array[Vertex] = g.zipWithIndex.map(p => Vertex(p._2, p._1.toArray) )
    val scc = new StronglyConnected(n, m, gv )
    val result = scc.run()
    println(result)
  }
}
