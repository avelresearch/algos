import java.util.Scanner
import scala.collection.mutable.ListBuffer

object StronglyConnected {

  def main(args: Array[String]): Unit = {

    type Vertex = Int

    val scanner = new Scanner(System.in);

    val n: Int = scanner.nextInt();

    val m: Int = scanner.nextInt();

    val vertexes: Array[ListBuffer[Vertex]] = Array.fill(n)(ListBuffer[Vertex]())
    //val visited: Array[Boolean] = new Array[Boolean](n)
    val tpOrder : Array[(Int, Int)] = Array.fill(n)( (0, 0) )

    for(i <- 1 to m)
    {
      val x = scanner.nextInt()
      val y = scanner.nextInt()
      vertexes(x - 1).append( y )
    }


    var d : Int = 0
    def inc() : Unit = d = d +1

    def explorer(v : Int) : Unit = {

      def exporerAcc(a: List[Int] ) : Unit = a match {
        case List() => Unit
        case h::tail => {

          inc()
          val r = explorer( h )

          val p = tpOrder(h - 1)
          if (p._1 == 0 || p._2 == 0) {
            inc()
            tpOrder.update(h - 1, (p._1, d))
          }

        }
      }

      //visited.update(v - 1, true)
      val s = tpOrder(v - 1)
      if (s._1 == 0) {
        tpOrder.update(v - 1, (d, 0))

        val n = vertexes(v - 1).filter(x => tpOrder(x - 1)._1 == 0 || tpOrder(x - 1)._2 == 0)
        if (n.length > 0)
          exporerAcc(n.toList)
        else {
          val p = tpOrder(v - 1)
          inc()
          tpOrder.update(v - 1, (p._1, d))
        }
      } else
        tpOrder.update(v - 1, (s._1, d))

    }

    def explorerAll() : Int = {

      def isDag() : Boolean =
        vertexes.zipWithIndex.forall(p => p._1.forall(c => tpOrder(p._2)._2 > tpOrder(c)._2  ) )

      var dag : Int = 0
      var flag : Boolean = true
      var d: Int = 0
      while(flag == true)
      {
        val freeVertexes = tpOrder.zipWithIndex.filter(e => e._1._1 == 0 && e._1._2 == 0)

        flag = freeVertexes.length > 0

        // Get first available vertex
        if (flag)
        {

          val vertex = freeVertexes(0)._2 + 1

          inc()
          explorer(vertex)

          val p = tpOrder(vertex - 1)
          if (p._2 == 0) {
            inc()
            tpOrder.update(vertex - 1, (p._1, d))
          }


        }

      }

      if ( isDag() ) 0 else 1
    }

    println( explorerAll() );
  }
}