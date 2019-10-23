package matching.tool

import sys.process._
import collection.mutable.ArrayBuffer

object Command {
  case class CommandResult(exitCode: Int, out: ArrayBuffer[String], err: ArrayBuffer[String])

  def exec(command: String): CommandResult = {
    val out, err = ArrayBuffer[String]()
    val logger = ProcessLogger(out += _, err += _)
    val code = Process(command).!(logger)
    CommandResult(code, out, err)
  }
}
