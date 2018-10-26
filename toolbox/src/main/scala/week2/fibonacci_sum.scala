import scala.annotation.tailrec

object fibonacci_sum {

  def main(args: Array[String]) {
    val N = Console.readLong()
    val f = new fibonacci_sum()
    val result = f.run(N)
    println( result  )
  }
}

class fibonacci_sum{
  def run(count: Long): Long = {
    @tailrec def _fib(count: Long, value: Long, accum: Long = 0): Long = count match {
      case 0 => accum
      case _ => _fib(count - 1, accum % 10  , (value + accum ) % 10 )
    }
    _fib(count, 1)
  }
}