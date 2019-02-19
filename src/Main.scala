package matching

import regexp._
import regexp.RegExp._
import regexp.RegExpParser._
import tool.{Analysis, File}
import scala.io.StdIn
import java.util.Calendar

object Main {
  def main(args: Array[String]) {
    if (args.isEmpty) interactiveTest()
    else if (args.length == 1) test(args(0))
    else throw new Exception("invalid argument")
  }

  def convertResult(result: Option[Int]): String = {
    result match {
      case Some(0) => "constant"
      case Some(1) => "linear"
      case Some(d) => s"O(n^${d}) polynomially"
      case None => "exponentially"
    }
  }

  def interactiveTest(sigma: Set[Char] = (' ' to '~').toSet) {
    var continue = true
    while (continue) {
      val regExpStr = StdIn.readLine()
      if (regExpStr.isEmpty) continue = false
      else {
        try {
          println(regExpStr)
          val r = RegExpParser(regExpStr)
          val result = calcBtrGrowthRate(r,sigma)
          println(convertResult(result))
        } catch {
          case e: ParseException => e.printStackTrace()
        }
        println()
      }
    }
  }

  def test(inputFile: String, sigma: Set[Char] = (' ' to '~').toSet) {
    val textFile = """(?:.*?)([^/]+)\.txt""".r
    val name = inputFile match {
      case textFile(name) => name
      case _ => throw new Exception("invalid file name")
    }
    val now = Calendar.getInstance()
    val timeStamp = f"${now.get(Calendar.YEAR)}-${now.get(Calendar.MONTH)}%02d-${now.get(Calendar.DATE)}%02d-${now.get(Calendar.HOUR_OF_DAY)}%02d-${now.get(Calendar.MINUTE)}%02d-${now.get(Calendar.SECOND)}%02d"

    val outputFile = File.makeFile(s"output/${name}_${timeStamp}.txt")

    def write(s: String = "") {
      println(s)
      outputFile.writeln(s)
    }

    val regExpStrs = File.loadFile(inputFile).getLines.toSeq
    val total = regExpStrs.length

    regExpStrs.zipWithIndex.foreach{ case (regExpStr,idx) =>
      println(s"${idx+1}/${total}")
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
        case e: ParseException => write(e.toString)
      }
      write()
    }
    outputFile.close()
  }
}
