import java.util.Scanner
import scala.collection.mutable.ListBuffer

/*
  Problem: Checking Consistency of CS Curriculum
  A Computer Science curriculum specifies the prerequisites for each course as
  a list of courses that should be taken before taking this course.
  You would like to perform a consistency check of the curriculum, that is,
  to check that there are no cyclic dependencies. For this, you construct
  the following directed graph: vertices correspond to courses,
  there is a directed edge (𝑢,𝑣) is the course 𝑢 should be taken before the course 𝑣.
  Then, it is enough to check whether the resulting graph contains a cycle.
 */

class Acyclicity(n: Int, m:Int, vertexes: Array[List[Int]]) {
  var d : Int = 0
  val tpOrder : Array[(Int, Int)] = Array.fill(n)( (0, 0) )
  def explorer(v : Int) : Unit = {
    def exporerAcc(a: List[Int] ) : Unit = a match {
      case List() => Unit
      case h::tail => {
        d = d + 1
        val r = explorer( h )
        val p = tpOrder(h - 1)
        if (p._1 == 0 || p._2 == 0) {
          d = d + 1
          tpOrder.update(h - 1, (p._1, d))
        }
        exporerAcc(tail)
      }
    }
    val s = tpOrder(v - 1)
    if (s._1 == 0) {
      tpOrder.update(v - 1, (d, 0))
      val n = vertexes(v - 1).filter(x => tpOrder(x - 1)._1 == 0 || tpOrder(x - 1)._2 == 0)
      if (n.length > 0) exporerAcc(n.toList)
      else {
        val p = tpOrder(v - 1)
        d = d + 1
        tpOrder.update(v - 1, (p._1, d))
      }
    } else tpOrder.update(v - 1, (s._1, d))
  }
  def explorerAll() : Int = {
    def isDag() : Boolean = {
      val result = vertexes.zipWithIndex.filter(x => x._1.size > 0)
      result.forall(p => p._1.forall(c => vertexes(c - 1).size == 0 || tpOrder(p._2)._2 >= tpOrder(c - 1)._2))
    }
    var dag : Int = 0
    var flag : Boolean = true
    while(flag == true)
    {
      val freeVertexes = tpOrder.zipWithIndex.filter(e => e._1._1 == 0 && e._1._2 == 0)
      flag = freeVertexes.length > 0
      if (flag)
      {
        val vertex = freeVertexes(0)._2 + 1
        d = d + 1
        explorer(vertex)
        val p = tpOrder(vertex - 1)
        if (p._2 == 0) {
          d = d + 1
          tpOrder.update(vertex - 1, (p._1, d))
        }
      }
    }
    val dgResult = isDag()
    if ( dgResult ) 0 else 1
  }
}

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

object Acyclicity {
  def main(args: Array[String]): Unit = {
    val reader = new InputReader()
    val (n,m) = reader.init()
    val graph = reader.getGraph(n, m)
    val ac = new Acyclicity(n, m, graph)
    println( ac.explorerAll() )
  }
}
