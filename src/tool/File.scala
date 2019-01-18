package matching.tool

import scala.io.Source
import java.nio.file.{Paths, Files}
import java.io.PrintWriter

object File {
  def loadFile(fileName: String): Source = {
    Source.fromFile(fileName)
  }

  def makeFile(fileName: String): File = {
    val file = Paths.get(fileName)
    Files.deleteIfExists(file)
    Files.createFile(file)
    new File(fileName)
  }
}

class File(fileName: String) extends PrintWriter(fileName) {
  var tab = " "*2

  def setTabSize(size: Int) = {
    tab = " "*size
  }

  def write(s: String, depth: Int = 0): Unit = {
    write(s"${tab*depth}${s}")
  }

  def writeln(s: String = "", depth: Int = 0) = {
    write(s"${tab*depth}${s}\n")
  }
}
