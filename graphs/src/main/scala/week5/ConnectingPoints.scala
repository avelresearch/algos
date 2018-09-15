import java.util.Scanner
import scala.collection.mutable.ListBuffer
import scala.util.Sorting

class ConnectingPoints(n: Int, x: Array[Int], y: Array[Int]) {
  def minimumDistance() : Double = {
    var X : ListBuffer[Double] = ListBuffer()

    val vSet : Array[Set[Int]] = Array.fill(n)( Set[Int]() )
    for(i <- 0 to n - 1)
      vSet(i) = vSet(i) + i


    def calcDist(p1: Int, p2: Int): Double = math.sqrt(math.pow(x(p1) - x(p2), 2) + math.pow(y(p1) - y(p2), 2))

    def inSameComponent(u: Int, v: Int) : Boolean =
      vSet.find(t => t.contains(u) ) match {
        case None => false
        case Some(s) => s.contains(v)
      }

    def union(u: Int, v: Int) : Unit = {
      val vSetIndx = vSet.zipWithIndex
      val uS = vSetIndx.find(c => c._1.contains(u)).get
      val vS = vSetIndx.find(c => c._1.contains(v)).get
      vSet(uS._2) = uS._1.union(vS._1)
      vSet(vS._2) = Set()
    }

    def iterate( l: List[(Int, Int, Double)]) : Unit = l match {
      case List() => Unit
      case h::tail => {
        val (u: Int, v: Int, w: Double) = h
        if ( inSameComponent(u, v) == false) {
          X += w
          union(v, u)
        }
        iterate(tail)
      }
    }

    val temp = ListBuffer[(Int, Int, Double)]()

    for (p1 <- 0 to n - 1) {
      for (p2 <- 0 to n - 1){
        if (p1 != p2 && temp.exists(p => p._1 == p2 && p._2 == p1) == false)
          temp.append( (p1, p2, calcDist(p1, p2) ) );
      }
    }

    val dist = temp.toArray

    Sorting.quickSort(dist)(Ordering.by[(Int, Int, Double), Double](_._3))

    iterate( dist.toList )

    X.foldLeft(0: Double)((s,v) => s + v)
  }
}

object ConnectingPoints {
  class InputReader {
    val scanner = new Scanner(System.in);
    def init() =scanner.nextInt()
    def getPoints(n: Int) : (Array[Int], Array[Int] ) = {
      val x: Array[Int] = Array.fill(n)(0)
      val y: Array[Int] = Array.fill(n)(0)

      for (i <- 0 to n - 1) {
        x(i) = scanner.nextInt()
        y(i) =  scanner.nextInt()
      }
      (x, y)
    }
  }

  def main(args: Array[String]): Unit = {
    val reader = new InputReader()
    val n: Int = reader.init()
    val (x, y) = reader.getPoints(n)
    val cp = new ConnectingPoints(n, x, y)
    val res = cp.minimumDistance()
    println(res)
  }
}
