object fibonacci {

  def main(args: Array[String]) {
    val N = Console.readInt
    val result = new fibonacci()
    println( result.run(N) )
  }
}

class fibonacci{
  def run(x: Int): BigInt = {
    def fiboAucc(x: Int, prev: BigInt = 0, next: BigInt = 1): BigInt = x match {
      case 0 => prev
      case 1 => next
      case _ => fiboAucc(x - 1, next, (next + prev))
    }
    fiboAucc(x)
  }
}