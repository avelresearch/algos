import java.util.Scanner
import scala.collection.mutable.ListBuffer

object Toposort {

  def main(args: Array[String]): Unit = {

    val scanner = new Scanner(System.in);

    val n: Int = scanner.nextInt();

    val m: Int = scanner.nextInt();

    val adj: Array[ListBuffer[Int]] = Array.fill(n)( ListBuffer[Int]())
    val inAdj: Array[ListBuffer[Int]] = Array.fill(n)( ListBuffer[Int]())

    for (i <- 1 to m) {
      val x = scanner.nextInt()
      val y = scanner.nextInt()
      adj(x - 1).append(y - 1)
      inAdj(y - 1).append(x - 1)
    }
    val order = toposort( adj, inAdj )
    order.foreach(o => print((o) + " "))
  }

  var order = List[Int]()

  def toposort(a: Array[ListBuffer[Int]], inAdj : Array[ListBuffer[Int]]) : List[Int] = {
    val used = new Array[Int](a.length)
    val as = inAdj.zipWithIndex.filter(v => v._1.length == 0)
    as.foreach(v => { if (used(v._2) == 0) { dfs(a, used, v._2) } });
    order
  }

  def dfs(adj: Array[ListBuffer[Int]], used: Array[Int], s : Int ) : Unit = {

    def explore(o: ListBuffer[Int]) : Unit = o match {
      case l if (l.isEmpty == false) => {
        if (used(l.head) == 0)
          dfs(adj, used, l.head)
        explore(l.tail)
        if (used(s) == 0) {
          used(s) = 1
          order = (s + 1)::order
        }
      }
      case _ => Unit
    }

    if (adj(s).isEmpty == false)
      explore( adj(s) )
    else if (used(s) == 0) {
      used(s) = 1 // forward()
      order = (s + 1)::order
    }

  }
}
