object lcm {

  def main(args: Array[String]) {
    val str = Console.readLine()
    val ab = str.split(" ").map(x => x.toLong)
    def gcd(a: Long, b: Long): Long = if(b ==0) a else gcd(b, a%b )
    val lcm = ab(0) * ab(1) / gcd( ab(0), ab(1) )
    println( lcm )
  }
}

class lcm {}