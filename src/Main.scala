package matching

import regexp._
import regexp.RegExp._
import tool.Analysis._
import collection.mutable.{Map => MTMap}
import tool.{IO, File, Debug}
import java.util.Date
import java.text.DateFormat
import scala.io.StdIn

object Main {
  class Settings() {
    var style = "raw"
    var timeout: Option[Int] = Some(10)

    override def toString(): String = {
      List(
        s"${"-"*3} settings ${"-"*27}",
        s"style: ${style}",
        s"timeout: ${if (timeout.isDefined) s"${timeout.get}s" else "disable"}",
        s"${"-"*40}"
      ).mkString("\n")
    }
  }

  def main(args: Array[String]) {
    def parseArgs(rawArgs: Array[String]): (Option[String], Settings) = {
      def parseOptions(options: List[String], setting: Settings = new Settings()): Settings = {
        options match {
          case "--style" :: style :: options =>
            val styleList = List("raw", "PCRE")
            if (styleList.contains(style)) {
              setting.style = style
              parseOptions(options, setting)
            } else throw new Exception(s"invalid style option: ${style}")
          case "--timeout" :: timeout :: options =>
            val t = try {
              timeout.toInt
            } catch {
              case e: NumberFormatException => throw new Exception(s"invalid timeout option: ${timeout}")
            }
            setting.timeout = if (t > 0) Some(t) else None
            parseOptions(options, setting)
          case "--debug" :: options =>
            Debug.debugModeGlobal = true
            parseOptions(options, setting)
          case Nil => setting
          case _ => throw new Exception("invalid option")
        }
      }

      val (args, optionStrs) = rawArgs.toList.span(!_.startsWith("--"))
      val inputFile = args match {
        case arg :: Nil => Some(arg)
        case Nil => None
        case _ => throw new Exception("invalid arguments")
      }

      (inputFile, parseOptions(optionStrs))
    }

    val (inputFile, settings) = parseArgs(args)

    println(settings)

    inputFile match {
      case Some(inputFile) => fileInputTest(inputFile, settings)
      case None => interactiveTest(settings)
    }
  }

  def test(regExpStr: String, settings: Settings): String = {
    def convertResult(result: Option[Int]): String = {
      result match {
        case Some(0) => "constant"
        case Some(1) => "linear"
        case Some(d) => s"polynomial, degree ${d}"
        case None => "exponential"
      }
    }

    try {
      val (r,option) = if (settings.style == "raw") {
        (RegExpParser(regExpStr), new PCREOption())
      } else if (settings.style == "PCRE") {
        RegExpParser.parsePCRE(regExpStr)
      } else throw new Exception("invalid style")
      runWithLimit(settings.timeout) {
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

  def interactiveTest(settings: Settings) {
    var continue = true
    while (continue) {
      println("please input expression. (input blank line to quit)")
      val regExpStr = StdIn.readLine()
      if (regExpStr.isEmpty) {
        continue = false
      } else {
        println(regExpStr)
        println(test(regExpStr, settings))
        println()
      }
    }
  }

  def fileInputTest(inputFile: String, settings: Settings) {
    val regExpStrs = IO.loadFile(inputFile).getLines.toSeq
    val total = regExpStrs.length

    val startTime = DateFormat.getDateTimeInstance().format(new Date())

    val dirName = s"output/${inputFile.replaceAll("""\W""","_")}_${startTime.replaceAll("""\W""","-")}"
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
      writeResult(test(regExpStr, settings))
      writeResult()
    }
    resultFile.close()
    resultListFile.close()

    val finishTime = DateFormat.getDateTimeInstance().format(new Date())


    val resultStrs = List("constant", "linear", "polynomial", "exponential", "timeout", "skipped", "error")
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
      if (r == "polynomial") {
        val polynomialResult = """polynomial, degree (\d*).*""".r
        result match {
          case polynomialResult(degree) =>
            val d = degree.toInt
            if (!degreeFiles.contains(d)) {
              IO.createDirectory(s"${detailDirNames("polynomial")}/degree_${d}")
              degreeFiles += d -> IO.createFile(s"${detailDirNames("polynomial")}/degree_${d}/result.txt")
              degreeListFiles += d -> IO.createFile(s"${detailDirNames("polynomial")}/degree_${d}/list.txt")
            }
            degreeFiles(d).writeln(regExp)
            degreeFiles(d).writeln(result)
            degreeFiles(d).writeln()
            degreeListFiles(d).writeln(regExp)
            degreeCount(d) += 1
        }
      }
    }

    summaryFile.writeln(s"input file : ${inputFile}")
    summaryFile.writeln(s"started at : ${startTime}")
    summaryFile.writeln(s"finished at: ${finishTime}")
    summaryFile.writeln()

    summaryFile.writeln(settings.toString)
    summaryFile.writeln()

    summaryFile.writeln(s"${"-"*3} result ${"-"*29}")
    summaryFile.writeln(f"${"total"}%-11s: ${total}")
    summaryFile.writeln()
    resultStrs.take(3).foreach{ resultStr =>
      summaryFile.writeln(f"${resultStr}%-11s: ${summaryCount(resultStr)}")
    }
    degreeCount.toSeq.sortBy(_._1).foreach{ case (degree,count) =>
      summaryFile.writeln(f"degree ${degree}: ${count}", 10)
    }
    resultStrs.drop(3).foreach{ resultStr =>
      summaryFile.writeln(f"${resultStr}%-11s: ${summaryCount(resultStr)}")
    }
    summaryFile.writeln(s"${"-"*40}")

    detailFiles.values.foreach(_.close())
    detailListFiles.values.foreach(_.close())
    degreeFiles.values.foreach(_.close())
    degreeListFiles.values.foreach(_.close())
    summaryFile.close()
  }
}
