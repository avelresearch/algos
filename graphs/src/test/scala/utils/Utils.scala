object SimpleTypeConverter {

  /** Converts text representation of graph to Array[List[Int]] */
  implicit def stringToArray(s: String) : Array[List[Int]] =
    s.stripMargin
      .split("\n")
      .filter(_.nonEmpty)
      .map(_.split(" "))
      .map(a => a.map(_.toInt))
      .map(_.toList)

}
