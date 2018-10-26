class gcd(a: Int,b: Int) {
  def calc(): Int = if(b ==0) a else gcd(b, a%b )
}

object gcd extends App {

  def main(args: Array[String]) {
    val str = Console.readLine()
    val ab = str.split(" ").map(x => x.toInt)
    val g = new gcd( ab(0), ab(1) )
    println( g.calc() )
  }

}
