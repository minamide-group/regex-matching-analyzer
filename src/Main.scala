package matching

import regexp._
import regexp.RegExp._
import tool.Analysis._
import collection.mutable.{Map => MTMap}
import tool.{IO, File}
import java.util.Date
import java.text.DateFormat
import scala.io.StdIn

object Main {
  def main(args: Array[String]) {
    if (args.isEmpty) interactiveTest()
    else if (args.length == 1) fileInputTest(args(0))
    else throw new Exception("invalid argument")
  }

  def test(regExpStr: String): String = {
    def convertResult(result: Option[Int]): String = {
      result match {
        case Some(0) => "constant"
        case Some(1) => "linear"
        case Some(d) => s"polynomially, degree ${d}"
        case None => "exponentially"
      }
    }

    val limit = 10
    try {
      val (r,option) = RegExpParser.parsePHP(regExpStr)
      runWithLimit(limit) {
        calcBtrGrowthRate(r,option)
      } match {
        case (Success(result),time) => s"${convertResult(result)}, ${time} ms"
        case (Failure(message),time) => s"skipped: ${message}, ${time} ms"
        case (Timeout(message),time) => s"timeout: ${message}, ${time} ms"
      }
    } catch {
      case e: RegExpParser.ParseException => s"error: ${e.message}"
    }
  }

  def interactiveTest() {
    var continue = true
    while (continue) {
      println("please input expression. (input blank line to quit)")
      val regExpStr = StdIn.readLine()
      if (regExpStr.isEmpty) continue = false
      else {
        println(regExpStr)
        println(test(regExpStr))
        println()
      }
    }
  }

  def fileInputTest(inputFile: String) {
    val regExpStrs = IO.loadFile(inputFile).getLines.toSeq
    val total = regExpStrs.length

    val timeStamp = DateFormat.getDateTimeInstance().format(new Date())
    val limit = 10

    val dirName = s"output/${inputFile.replaceAll("""\W""","_")}_${timeStamp.replaceAll("""\W""","-")}"
    IO.createDirectory(dirName)

    val resultName = s"${dirName}/result.txt"
    val resultListName = s"${dirName}/list.txt"
    val resultFile = IO.createFile(resultName)
    val resultListFile = IO.createFile(resultListName)
    def writeResult(s: String = "") {
      println(s)
      resultFile.writeln(s)
    }

    regExpStrs.zipWithIndex.foreach{ case (regExpStr,idx) =>
      println(s"${idx+1}/${total}")
      writeResult(regExpStr)
      resultListFile.writeln(regExpStr)
      writeResult(test(regExpStr))
      writeResult()
    }
    resultFile.close()
    resultListFile.close()


    val resultStrs = List("constant", "linear", "polynomially", "exponentially", "timeout", "skipped", "error")
    val detailDirNames = resultStrs.map(resultStr => resultStr -> s"${dirName}/${resultStr}").toMap
    resultStrs.foreach(resultStr => IO.createDirectory(detailDirNames(resultStr)))
    val detailFiles = resultStrs.map(resultStr => resultStr -> IO.createFile(s"${detailDirNames(resultStr)}/result.txt")).toMap
    val detailListFiles = resultStrs.map(resultStr => resultStr -> IO.createFile(s"${detailDirNames(resultStr)}/list.txt")).toMap
    val degreeFiles = MTMap[Int, File]()
    val degreeListFiles = MTMap[Int, File]()
    val summaryFile = IO.createFile(s"${dirName}/summary.txt")
    val summaryCount = MTMap[String, Int]().withDefaultValue(0)
    val degreeCount = MTMap[Int, Int]().withDefaultValue(0)

    val results = IO.loadFile(resultName).getLines.filter(_.nonEmpty).sliding(2,2)
    results.foreach{ case List(regExp, result) =>
      val r = result.takeWhile(_.isLetter)
      detailFiles(r).writeln(regExp)
      detailFiles(r).writeln(result)
      detailFiles(r).writeln()
      detailListFiles(r).writeln(regExp)
      summaryCount(r) += 1
      if (r == "polynomially") {
        val polynomialResult = """polynomially, degree (\d*).*""".r
        result match {
          case polynomialResult(degree) =>
            val d = degree.toInt
            if (!degreeFiles.contains(d)) {
              IO.createDirectory(s"${detailDirNames("polynomially")}/degree_${d}")
              degreeFiles += d -> IO.createFile(s"${detailDirNames("polynomially")}/degree_${d}/result.txt")
              degreeListFiles += d -> IO.createFile(s"${detailDirNames("polynomially")}/degree_${d}/list.txt")
            }
            degreeFiles(d).writeln(regExp)
            degreeFiles(d).writeln(result)
            degreeFiles(d).writeln()
            degreeListFiles(d).writeln(regExp)
            degreeCount(d) += 1
        }
      }
    }

    summaryFile.writeln(s"input file: ${inputFile}")
    summaryFile.writeln(s"date: ${timeStamp}")
    summaryFile.writeln()

    summaryFile.writeln(s"${"-"*3} settings ${"-"*27}")
    summaryFile.writeln(s"time limit: ${limit}s")
    summaryFile.writeln(s"${"-"*40}")
    summaryFile.writeln()

    summaryFile.writeln(s"${"-"*3} result ${"-"*29}")
    summaryFile.writeln(f"${"total"}%-13s: ${total}")
    summaryFile.writeln()
    resultStrs.take(3).foreach{ resultStr =>
      summaryFile.writeln(f"${resultStr}%-13s: ${summaryCount(resultStr)}")
    }
    degreeCount.toSeq.sortBy(_._1).foreach{ case (degree,count) =>
      summaryFile.writeln(f"degree ${degree}: ${count}", 10)
    }
    resultStrs.drop(3).foreach{ resultStr =>
      summaryFile.writeln(f"${resultStr}%-13s: ${summaryCount(resultStr)}")
    }
    summaryFile.writeln(s"${"-"*40}")

    detailFiles.values.foreach(_.close())
    detailListFiles.values.foreach(_.close())
    degreeFiles.values.foreach(_.close())
    degreeListFiles.values.foreach(_.close())
    summaryFile.close()
  }
}
