object Fractional_knapsack {

  def main(args: Array[String]) {

    val inputNW = Console.readLine()

    val nw = inputNW.split(" ").map(x => x.toInt)

    val vwList = getValuesWeight( nw(0), List() ).sortWith(_._3 > _._3)

    val maxValue = getMaxValue( vwList, nw(1), 0)

    println( maxValue )
  }

  def getMaxValue(items: List[(Int, Int, Double)], remainingWeight : Int, accValue: Double): Double = {
    if (remainingWeight == 0) accValue
    else {
      items match {
        case List() => accValue
        case h::tail => {
          if ((remainingWeight - h._2) > 0)
            getMaxValue(tail, remainingWeight - h._2, accValue + h._1)
          else
            getMaxValue( tail, 0,  accValue + remainingWeight * h._3)
        }
      }
    }
  }

  def getValuesWeight(n: Int, acc:List[(Int, Int, Double)]) : List[(Int, Int, Double)] = {
    if (n == 0) acc
    else
    {
      val pair = Console.readLine()
      val result = pair.split(" ").map(x => x.toInt)
      val d : Double = (result(0).toDouble / result(1)).toDouble
      getValuesWeight(n - 1, (result(0), result(1), d )::acc )
    }
  }
}

class Fractional_knapsack {}
