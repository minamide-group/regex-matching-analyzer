package matching

import regexp._
import regexp.RegExp._
import tool.Analysis._
import tool.File
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

  def interactiveTest() {
    var continue = true
    while (continue) {
      println("please input expression. (input blank line to quit)")
      val regExpStr = StdIn.readLine()
      if (regExpStr.isEmpty) continue = false
      else {
        try {
          println(regExpStr)
          val (r,option) = RegExpParser.parsePHP(regExpStr)
          val result = calcBtrGrowthRate(r,option)
          println(convertResult(result))
        } catch {
          case e: RegExpParser.ParseException => e.printStackTrace()
        }
        println()
      }
    }
  }

  def test(inputFile: String) {
    val textFile = """(?:.*?)([^/]+)\.txt""".r
    val name = inputFile match {
      case textFile(name) => name
      case _ => throw new Exception("invalid file name")
    }
    val now = Calendar.getInstance()
    val timeStamp = f"${now.get(Calendar.YEAR)}-${now.get(Calendar.MONTH)+1}%02d-${now.get(Calendar.DATE)}%02d-${now.get(Calendar.HOUR_OF_DAY)}%02d-${now.get(Calendar.MINUTE)}%02d-${now.get(Calendar.SECOND)}%02d"

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
        val (r,option) = RegExpParser.parsePHP(regExpStr)
        runWithLimit(10000) {
          calcBtrGrowthRate(r,option)
        } match {
          case (Success(result),time) =>
            write(s"${convertResult(result)}, ${time} ms")
          case (Failure(message),time) =>
            write(s"error: ${message}, ${time} ms")
          case (Timeout(message),time) =>
            write(s"timeout: ${message}, ${time} ms")
        }
      } catch {
        case e: RegExpParser.ParseException => write(s"error: ${e.message}")
      }
      write()
    }
    outputFile.close()
  }
}
