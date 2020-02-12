package matching

sealed trait TestResult
case class Success(
  growthRate: Option[Int],
  witness: Witness[Char],
  approximated: Boolean,
  size: Int,
  time: Long
) extends TestResult {
  override def toString(): String = {
    var str = if (approximated) "at most " else ""
    str += (growthRate match {
      case Some(0) => "constant"
      case Some(1) => "linear"
      case Some(d) => s"polynomial, degree ${d}"
      case None => s"exponential"
    })

    if (witness != Witness.empty) str += s", witness: ${witness}"

    str += s", time: ${time} ms"

    str
  }

  def getTime(): String = {
    s"${size}, ${time / 1000.0}"
  }
}

case class Skipped(message: String) extends TestResult {
  override def toString(): String = {
    s"skipped: ${message}"
  }
}

case class Error(message: String) extends TestResult {
  override def toString(): String = {
    s"error: ${message}"
  }
}

case object Timeout extends TestResult {
  override def toString(): String = {
    s"timeout"
  }
}
