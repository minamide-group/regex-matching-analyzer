package matching

import tool.IO

case class Witness[A](var separators: Seq[Seq[A]], pumps: Seq[Seq[A]]) {
  override def toString(): String = {
    var ss = IndexedSeq[String]()
    if (pumps.nonEmpty) {
      val escapedSeparators = separators.map(_.map(IO.escape).mkString)
      val escapedPumps = pumps.map(_.map(IO.escape).mkString("\u2772","","\u2773^n"))
      ss :+= escapedSeparators.head
      escapedPumps.zip(escapedSeparators.tail).foreach{
        case (p,s) => ss ++= Seq(p,s)
      }
    }
    ss.filter(_.nonEmpty).mkString(" ")
  }
}

object Witness {
  def empty[A]: Witness[A] = Witness(Seq(),Seq())
}
