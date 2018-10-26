object fibonacci_huge {

  def main(args: Array[String]) {
    val N = Console.readInt
    val f = new fibonacci_huge()
    val result = f.run(N)
    println( result  )
  }
}

class fibonacci_huge{
  def run(x: Int): BigInt = {
    def fiboAucc(x: Int, prev: BigInt = 0, next: BigInt = 1): BigInt = x match {
      case 0 => prev
      case 1 => next
      case _ => fiboAucc(x - 1, next, (next + prev)) % 10
    }
    fiboAucc(x)
  }
}