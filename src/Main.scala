package matching

import regexp._
import regexp.RegExp._
import tool.{File, Analysis, Debug}

object Main {
  def main(args: Array[String]) {
    if (args.length == 2) test(args(0),args(1))
    else throw new Exception("invalid argument")
  }

  def test(input: String, output: String, sigma: Set[Char] = (' ' to '~').toSet) {
    def convertResult(result: Option[Int]): String = {
      result match {
        case Some(0) => "constant"
        case Some(1) => "linear"
        case Some(d) => s"O(n^${d}) polynomially"
        case None => "exponentially"
      }
    }

    val outputFile = File.makeFile(output)

    def write(s: String = "") {
      Debug.debug(s)
      outputFile.writeln(s)
    }

    val regExpStrs = File.loadFile(input).getLines.toSeq
    val total = regExpStrs.length

    regExpStrs.zipWithIndex.foreach{ case (regExpStr,idx) =>
      Debug.debug(s"${idx+1}/${total}")
      write(regExpStr)
      try {
        val r = RegExpParser(regExpStr)
        Analysis.runWithLimit(10000) {
          calcBtrGrowthRate(r,sigma)
        } match {
          case (Some(result),time) =>
            write(s"${convertResult(result)}, ${time} ms")
          case (None,time) =>
            write(s"timeout, ${time} ms")
        }
      } catch {
        case _: Exception => write("parse error")
      }
      write()
    }
    outputFile.close()
  }
}
