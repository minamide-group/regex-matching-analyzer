import regexp._

object Main {
  def main(args: Array[String]) {
    val r = RegExpParser("a*a*")
    val result = r.calcGrowthRate()
    println(result match {
      case Some(0) => "finitely"
      case Some(d) => s"O(n^${d}) polynomially"
      case None => "exponentially"
    })
  }
}
