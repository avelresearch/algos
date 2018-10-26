object covering_segments {

  def main(args: Array[String]) {

    val input = Console.readLine()

    val N = input.toLong

    val t : List[(Long, Long)] = getInput(N, List() )

    val sortedTimes = t.sortWith(_._2 < _._2)

    var result : List[Long] = getBestCollectTime(sortedTimes, 0, List())

    println( result.length )

    result.sortWith(_ < _).foreach( x => print(x + " ")  )
  }

  def getBestCollectTime( times : List[(Long, Long)], startTime : Long, acc: List[Long]) : List[Long] =  {
    if (startTime == 0) {
      times match {
        case List() => acc
        case h::tail => getBestCollectTime( tail, h._2, h._2::acc)
      }
    }
    else {
      times match {
        case List() => acc
        case h::tail => if ((h._1 <= startTime) && (h._2 >= startTime))
          getBestCollectTime( tail, startTime, acc)
        else
          getBestCollectTime( tail, h._2, h._2::acc)
      }
    }
  }

  def getInput(x:Long, acc: List[(Long, Long)]) : List[(Long, Long)] = {
    if (x == 0) acc
    else {
      val input = Console.readLine()
      val t = input.split(" ").map(x => x.toLong)
      getInput(x - 1, ( t(0), t(1))::acc  )
    }
  }

}

class covering_segments {}
