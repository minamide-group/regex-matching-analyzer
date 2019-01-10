package tool

object Debug {
  def info(name: String)(props: (String, Any)*) {
    val lineLength = 50
    println(s"--- ${name} ${"-"*((lineLength - (5 + name.length)).max(0))}")
    props.foreach{ case (k,v) =>
      println(s"${k}: ${v}")
    }
    println("-"*lineLength)
  }

  def time[A](name: String)(proc: => A): A = {
    val start = System.currentTimeMillis()
    val a = proc
    val finish = System.currentTimeMillis()
    println(s"[time] ${name}: ${finish - start} ms")
    a
  }

  implicit class Tmp[A](a: A) {
    def tmp(f: A => Unit): A = {
      f(a)
      a
    }
  }
}
