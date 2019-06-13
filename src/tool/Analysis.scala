package matching.tool

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

object Analysis {
  sealed trait AnalysisResult[+A]
  case class Success[A](result: A) extends AnalysisResult[A]
  case class Failure(message: String) extends AnalysisResult[Nothing]
  case class Timeout(message: String) extends AnalysisResult[Nothing]

  case class InterruptedNotification(message: String) extends Exception(message)

  def checkInterrupted(message: String = "") = {
    if (Thread.currentThread().isInterrupted()) throw InterruptedNotification(message)
  }

  def runWithLimit[A](limit: Option[Int])(proc: => A): (AnalysisResult[A], Long) = {
    var result: (AnalysisResult[A], Long) = (Failure(""), 0)

    val thread = new Thread {
      override def run() {
        val start = System.currentTimeMillis()
        val a = try {
          Success(proc)
        } catch {
          case e: InterruptedNotification =>
            Debug.debug(s"interrupted: ${e.message}")
            Timeout(e.message)
          case e: Exception =>
            Failure(e.getMessage())
        }
        val finish = System.currentTimeMillis()
        result = (a, finish - start)
      }
    }

    val future = Future {
      thread.start()
      thread.join()
      result
    }

    try {
      Await.result(future, if (limit.isDefined) Duration(limit.get, SECONDS) else Duration.Inf)
    } catch {
      case _: TimeoutException =>
        thread.interrupt()
        thread.join()
        result match {
          case (Timeout(_),_) => result
          case (_,time) => (Timeout("unknown."), time)
        }
    }
  }
}
