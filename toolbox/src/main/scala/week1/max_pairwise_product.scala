import scala.collection.mutable.ListBuffer

case class Pair(x: Int, y: Int){
  def compare(z: Int) : Pair = {
    if (z > x) Pair(z, x) 
    else if (z > y) Pair(x, z)
    else this
  } 
}

class max_pairwise_product(input: Seq[Int]) {
  var pair = Pair(0, 0)
  def run() : BigInt = {
    if (input.length == 2)
      pair = Pair( input(0), input(1) )  
    else {
      input.foreach(z => {
        pair = pair.compare(z)
      })
    }
    BigInt(pair.x) * BigInt(pair.y)
  }
}

object max_pairwise_product extends App {
  val scanner = new java.util.Scanner(System.in)
  val count = scanner.nextLine()  
  var pair = Pair(0, 0)
  (1 to count.toInt).foreach{ _ => 
    val z = scanner.nextInt()
    pair = pair.compare(z)
  }
  val result = BigInt(pair.x) * BigInt(pair.y)
  println(result)
}
