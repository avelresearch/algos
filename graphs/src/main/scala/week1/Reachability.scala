import java.util.Scanner

import scala.collection.mutable.ListBuffer

class Reachability(n: Int, m: Int, vertexes: Array[List[Int]], from: Int, to: Int) {
  val visited: Array[Boolean] = new Array[Boolean](n)
  var found = false

  def explorer(v: Int): Int = {

    def exporerAcc(a: List[Int]): Boolean = a match {
      case List() => found
      case h :: tail => {

        if (!visited(h - 1))
          explorer(h)

        if (!found)
          exporerAcc(tail)

        found
      }
    }

    visited.update(v - 1, true)
    if (v == to) found = true
    else {
      val n = vertexes(v - 1)
      exporerAcc(n)
    }
    if (found) 1 else 0
  }
}

object Reachability {
  def main(args: Array[String]): Unit = {
    val scanner = new Scanner(System.in);
    val (n, m) = (scanner.nextInt(), scanner.nextInt() )
    val vertexes: Array[ListBuffer[Int]] = Array.fill(n)(ListBuffer[Int]())
    (1 to m).foreach(_ => {
      val x = scanner.nextInt()
      val y = scanner.nextInt()
      vertexes(x - 1).append(y)
      vertexes(y - 1).append(x)
    })
    val (from, to) = (scanner.nextInt(), scanner.nextInt())
    val rc = new Reachability(n, m, vertexes.map(_.toList), from, to)
    val result = rc.explorer(from)
    println(result)
  }
}

