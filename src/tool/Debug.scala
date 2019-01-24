package matching.tool

import Console._

object Debug {
  val debugModeGlobal = true

  def debug(x: Any) {
    val DEBUG = "\u001b[38;5;111m"
    if (debugModeGlobal) println(s"${DEBUG}${x}${RESET}")
  }

  def info(name: String, debugModeLocal: Boolean = true)(props: (String, Any)*) {
    if (debugModeLocal) {
      val lineLength = 50
      debug(s"--- ${name} ${"-"*((lineLength - (5 + name.length)).max(0))}")
      props.foreach{ case (k,v) =>
        debug(s"${k}: ${v}")
      }
      debug("-"*lineLength)
    }
  }

  def time[A](name: String, debugModeLocal: Boolean = true)(proc: => A): A = {
    if (debugModeLocal) {
      val start = System.currentTimeMillis()
      val a = proc
      val finish = System.currentTimeMillis()
      debug(s"${name}: ${finish - start} ms")
      a
    } else proc
  }

  implicit class Tmp[A](a: A) {
    def tmp(f: A => Unit): A = {
      f(a)
      a
    }
  }
}
