package matching

import regexp._
import regexp.RegExp._

object Main {
  def main(args: Array[String]) {
    def printResult(result: Option[Int]) {
      println(result match {
        case Some(1) => s"O(n) polynomially"
        case Some(d) => s"O(n^${d}) polynomially"
        case None => "exponentially"
      })
    }

    val r = RegExpParser("a*a*")
    val sigma = (' ' to '~').toSet
    printResult(calcGrowthRate(r, sigma))
    printResult(calcBtrGrowthRate(r, sigma))
  }
}
