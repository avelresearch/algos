class APlusB(input: String) {
  def run() : BigInt = (input.split(" ").map( x => BigInt(x)) take 2).sum
}

object APlusB extends App {
  override def main(args: Array[String]): Unit = {
    val scanner = new java.util.Scanner(System.in)
    val line = scanner.nextLine()
    val result = {
      val obj = new APlusB(line)
      obj.run()
    }
    System.out.print(result)
  }
}
