object Change {

  def main(args: Array[String]) {
    val input = Console.readLine()
    val amount = input.toInt
    val count = countChange(amount, List(1, 5, 10) )
    println( count )
  }

  def countChange(money: Int, coins: List[Int]): Int = {
    def countAcc(moneyLeft: Int, counter: Int, cs: List[Int]) : Int = {
      if (moneyLeft == 0) counter
      else {
        val greedy  = {
          for {
            c <- cs
            if (moneyLeft - c) >= 0
          } yield c
        }
        countAcc(moneyLeft - greedy(0), counter + 1, cs)
      }
    }
    val sorterCoins = coins.sortWith(_ > _)
    countAcc(money, 0, sorterCoins)
  }
}


class Change {}
