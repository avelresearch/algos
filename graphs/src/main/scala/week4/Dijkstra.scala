import java.util.Scanner
import DijkstraTypes.{Edges, QueueItem}
import scala.collection.mutable.ListBuffer

object DijkstraTypes {
  type Edges = Array[List[Int]]
  type QueueItem = (Int, Int)
}

class Dijkstra(n: Int, adj: Edges, cost: Edges) {
  private val INFINITY = Int.MaxValue / 2
  private val UNDEFINED = Int.MaxValue / 5
  private case class Queue(s: Int) {
    def isEmpty = items.isEmpty
    val items: ListBuffer[QueueItem] =
      List.fill(n)().zipWithIndex.map(v => if (s != v._2) (v._2, INFINITY) else (s, 0)).to
    def changePriority(v: Int, alt: Int): Unit = items.find(x => x._1 == v) match {
      case None => items.append((v, alt))
      case Some(a) => { items -= a; items.append((a._1, alt)) }
    }
    def extractMin(): Int = {
      val m = items.map(x => x._2).min
      items.find(p => p._2 == m) match {
        case None => -1
        case Some(v) => { items -= v; v._1}
      }
    }
  }
  def distance(s: Int, t: Int): Int = {
    val dist: Array[Int] = Array.fill(n)(INFINITY)
    val prev: Array[Int] = Array.fill(n)(INFINITY)
    val queue = Queue(s)
    def relax(adjU: List[Int], costU: List[Int], u: Int): Unit = adjU match {
      case List() => Unit
      case v :: tail => {
        val alt: Int = costU.head + dist(u)
        if (alt < dist(v)) {
          dist(v) = alt
          prev(v) = u
          queue.changePriority(v, alt)
        }
        relax(tail, costU.tail, u)
      }
    }
    dist(s) = 0
    while (queue.isEmpty == false) {
      val u = queue.extractMin()
      relax(adj(u), cost(u), u)
    }
    if (dist(t) != INFINITY) dist(t) else -1
  }
}

object Dijkstra {
  class InputReader {
    val scanner = new Scanner(System.in);
    def init() = (scanner.nextInt(), scanner.nextInt())
    def getGraph(m: Int, n: Int): (Edges, Edges) = {
      val cost: Array[ListBuffer[Int]] = Array.fill(n)(ListBuffer[Int]())
      val adj: Array[ListBuffer[Int]] = Array.fill(n)(ListBuffer[Int]())
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
    val (adj, cost) = reader.getGraph(m, n)
    val (s, t) = (reader.scanner.nextInt() - 1, reader.scanner.nextInt() - 1)
    val d = new Dijkstra(n, adj, cost)
    val res = d.distance(s, t)
    println(res)
  }
}
