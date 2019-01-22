package matching.tool

import Console._

object Debug {
  val debug = false

  def debug(x: Any) {
    val DEBUG = "\u001b[38;5;111m"
    if (debug) println(s"${DEBUG}${x}${RESET}")
  }

  def info(name: String)(props: (String, Any)*) {
    val lineLength = 50
    debug(s"--- ${name} ${"-"*((lineLength - (5 + name.length)).max(0))}")
    props.foreach{ case (k,v) =>
      debug(s"${k}: ${v}")
    }
    debug("-"*lineLength)
  }

  def time[A](name: String)(proc: => A): A = {
    val start = System.currentTimeMillis()
    val a = proc
    val finish = System.currentTimeMillis()
    debug(s"${name}: ${finish - start} ms")
    a
  }

  implicit class Tmp[A](a: A) {
    def tmp(f: A => Unit): A = {
      f(a)
      a
    }
  }
}
