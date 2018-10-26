class gcd {
  def calc(a: Int,b: Int): Int = if(b ==0) a else calc(b, a%b )
}

object gcd {

  def main(args: Array[String]) {
    val str = Console.readLine()
    val ab = str.split(" ").map(x => x.toInt)
    val g = new gcd()
    println( g.calc(ab(0), ab(1)) )
  }

}
