package matching

import regexp._
import regexp.RegExp._
import tool.Analysis
import tool.Analysis._
import transition._
import tool.{IO, File, Debug}
import io.StdIn
import collection.mutable.{Map => MTMap}
import java.util.Date
import java.text.DateFormat

object Main {
  class Settings() {
    var style: RegExpStyle = Raw
    var method: Option[BacktrackMethod] = Some(Lookahead)
    var timeout: Option[Int] = Some(10)

    override def toString(): String = {
      List(
        s"${"-"*3} settings ${"-"*27}",
        s"style: ${style}",
        s"method: ${if (method.isDefined) s"${method.get}" else "Exhaustive"}",
        s"timeout: ${if (timeout.isDefined) s"${timeout.get}s" else "disabled"}",
        s"${"-"*40}"
      ).mkString("\n")
    }
  }

  def main(args: Array[String]) {
    def parseArgs(rawArgs: Array[String]): (Option[String], Settings) = {
      def parseOptions(options: List[String], setting: Settings = new Settings()): Settings = {
        options match {
          case "--style" :: style :: options =>
            setting.style = style match {
              case "raw" => Raw
              case "PCRE" => PCRE
              case _ => throw new Exception(s"invalid style option: ${style}")
            }
            parseOptions(options, setting)
          case "--method" :: method :: options =>
            setting.method = method match {
              case "Lookahead" => Some(Lookahead)
              case "SubsetPrune" => Some(SubsetPrune)
              case "Nondeterminism" => Some(Nondeterminism)
              case "Exhaustive" => None
              case _ => throw new Exception(s"invalid method option: ${method}")
            }
            parseOptions(options, setting)
          case "--timeout" :: timeout :: options =>
            val t = try {
              timeout.toInt
            } catch {
              case e: NumberFormatException =>
                throw new Exception(s"invalid timeout option: ${timeout}")
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

  def test(regExpStr: String, settings: Settings): TestResult = {
    try {
      val (r,option) = if (settings.style == "raw") {
        (RegExpParser(regExpStr), new PCREOption())
      } else if (settings.style == "PCRE") {
        RegExpParser.parsePCRE(regExpStr)
      } else throw new Exception("invalid style")
      runWithLimit(settings.timeout) {
        getTransducerSize(r,option)
      } match {
        case (Analysis.Success(ruleSize),_) =>
          runWithLimit(settings.timeout) {
            calcTimeComplexity(r,option,settings.method)
          } match {
            case (Analysis.Success((growthRate, witness)),time) =>
              Success(growthRate, witness, ruleSize, time)
            case (Analysis.Failure(message),_) => Skipped(message)
            case (Analysis.Timeout(_),_) => Timeout
          }
        case (Analysis.Failure(message),_) => Skipped(message)
        case (Analysis.Timeout(_),_) => Timeout
      }
    } catch {
      case e: RegExpParser.ParseException => Error(e.message)
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
    val timeName = s"${dirName}/time.txt"
    val resultFile = IO.createFile(resultName)
    val resultListFile = IO.createFile(resultListName)
    val timeFile = IO.createFile(timeName)

    def writeResult(s: String = "") {
      println(s)
      resultFile.writeln(s)
    }

    def writeTime(s: String) {
      timeFile.writeln(s)
    }

    regExpStrs.zipWithIndex.foreach{ case (regExpStr,idx) =>
      println(s"${idx+1}/${total}")
      writeResult(regExpStr)
      resultListFile.writeln(regExpStr)
      test(regExpStr, settings) match {
        case s: Success =>
          writeResult(s.toString())
          writeTime(s.getTime())
        case result =>
          writeResult(result.toString())
      }
      writeResult()
    }

    resultFile.close()
    resultListFile.close()
    timeFile.close()

    val finishTime = DateFormat.getDateTimeInstance().format(new Date())

    val resultStrs = List("constant", "linear", "polynomial", "exponential", "timeout", "skipped", "error")
    val detailDirNames = resultStrs.map(resultStr => resultStr -> s"${dirName}/${resultStr}").toMap
    resultStrs.foreach(resultStr => IO.createDirectory(detailDirNames(resultStr)))
    val detailFiles = resultStrs.map(resultStr =>
      resultStr -> IO.createFile(s"${detailDirNames(resultStr)}/result.txt")
    ).toMap
    val detailListFiles = resultStrs.map(resultStr =>
      resultStr -> IO.createFile(s"${detailDirNames(resultStr)}/list.txt")
    ).toMap
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
              degreeFiles += d -> IO.createFile(
                s"${detailDirNames("polynomial")}/degree_${d}/result.txt")
              degreeListFiles += d -> IO.createFile(
                s"${detailDirNames("polynomial")}/degree_${d}/list.txt")
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
