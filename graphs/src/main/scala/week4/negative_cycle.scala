import java.util.Scanner

import NegativeCycleTypes.Edges

import scala.collection.mutable.ListBuffer

object NegativeCycleTypes {
  type Edges = Array[List[Int]]
}

class NegativeCycle(n: Int, m: Int, adj: Edges, cost: Edges){
  def run() : Int = {

    val   s: Int = 0
    val   INFINITY    = Int.MaxValue / 2
    val   NIL   = Int.MaxValue / 5

    val dist:Array[Int] = Array.fill(n)(INFINITY)
    val prev:Array[Int] = Array.fill(n)(NIL)

    def relax(u: Int, v: Int, c: Int) : Boolean = {
      val alt : Int = c + dist(u)
      if ( alt < dist(v) )
      {
        dist(v) = alt
        prev(v) = u
        true
      } else false
    }

    dist(s) = 0

    def iter() : Boolean = {

      var result : Boolean = false

      adj.zipWithIndex.foreach(p => {
        val u = p._2
        p._1.zipWithIndex.foreach(e => {
          val v = e._1
          // if at least one relaxation during iteration
          if (result == false)
            result = relax(u, v, cost(u)(e._2) )

        });
      });

      result
    };

    def repeat(times : Int, f: => Boolean, result: Boolean) : Boolean = times match {
      case 0 => result
      case t => {
        val r: Boolean = f
        repeat(times - 1, f, r)
      }
    }

    val negative = repeat( adj.length, iter, false)

    if ( negative == false) 0 else 1
  }
}

object negative_cycle {
  class InputReader {
    val scanner = new Scanner(System.in);
    def init() = (scanner.nextInt(), scanner.nextInt())
    def getGraph(n : Int, m: Int) : (Edges, Edges) = {
      val adj = Array.fill(n)(ListBuffer[Int]())
      val cost = Array.fill(n)(ListBuffer[Int]())
      for (i <- 1 to m) {
        val (x, y, w) = (scanner.nextInt(), scanner.nextInt(), scanner.nextInt())
        adj(x - 1).append(y - 1)
        cost(x - 1).append(w)
      }
      (adj.map(_.toList), cost.map(_.toList))
    }
  }

  def main(args: Array[String]): Unit = {
    val reader = new InputReader()
    val (n: Int, m: Int) = reader.init()
    val (adj, cost) = reader.getGraph(n, m)

    val neg = new NegativeCycle(n, m, adj, cost)
    println( neg.run() )
  }

}


