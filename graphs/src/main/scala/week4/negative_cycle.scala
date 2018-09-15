import java.util.Scanner
import NegativeCycleTypes.Edges
import scala.collection.mutable.ListBuffer

/*
  Problem: Detecting Anomalies in Currency Exchange Rates
  Problem Introduction
  You are given a list of currencies c1,c2,...,cn together with a
  list of ex- change rates: rij is the number of units of currency cj that one
  gets for one unit of ci. You would like to check whether it is possible to start with one unit of some currency,
  perform a sequence of exchanges, and get more than one unit of the same currency.
  In other words, you would like to findcurrenciesci1,ci2,...,cik suchthatri1,i2 ·ri2,i3 ·rik−1,ik,rik,i1 >1.

  For this, you construct the following graph: vertices are currencies c1, c2, . . . , cn,
  the weight of an edge from ci to cj is equal to − log rij .
  There it suf- fices to check whether is a negative cycle in this graph.
  Indeed, assume that a cycle ci → cj → ck → ci has negative weight.
  This means that −(logcij + logcjk + logcki) < 0 and hence logcij + logcjk + logcki > 0.
  This, in turn, means that

    rijrjkrki =2logcij2logcjk2logcki =2logcij+logcjk+logcki >1

  Problem Description
  Task. Given an directed graph with possibly negative edge weights
  and with n vertices and m edges, check whether it contains a cycle of negative weight.
 */

object NegativeCycleTypes {
  type Edges = Array[List[Int]]
}

class NegativeCycle(n: Int, m: Int, adj: Edges, cost: Edges) {
  val INFINITY = Int.MaxValue / 2
  val NIL = Int.MaxValue / 5

  def run(): Int = {
    val s: Int = 0
    val dist: Array[Int] = Array.fill(n)(INFINITY)
    val prev: Array[Int] = Array.fill(n)(NIL)

    def relax(u: Int, v: Int, c: Int): Boolean = {
      val alt: Int = c + dist(u)
      if (alt < dist(v)) {
        dist(v) = alt
        prev(v) = u
        true
      } else false
    }

    dist(s) = 0

    def iter(): Boolean = {
      var result: Boolean = false
      adj.zipWithIndex.foreach(p => {
        val u = p._2
        p._1.zipWithIndex.foreach(e => {
          val v = e._1
          // if at least one relaxation during iteration
          if (result == false)
            result = relax(u, v, cost(u)(e._2))

        });
      });

      result
    };

    def repeat(times: Int, f: => Boolean, result: Boolean): Boolean = times match {
      case 0 => result
      case t => {
        val r: Boolean = f
        repeat(times - 1, f, r)
      }
    }

    val negative = repeat(adj.length, iter, false)
    if (negative == false) 0 else 1
  }
}

object negative_cycle {

  class InputReader {
    val scanner = new Scanner(System.in);

    def init() = (scanner.nextInt(), scanner.nextInt())

    def getGraph(n: Int, m: Int): (Edges, Edges) = {
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
    println(neg.run())
  }

}


