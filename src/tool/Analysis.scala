package matching.tool

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

object Analysis {
  case class InterruptedNotification(message: String) extends Exception(message)

  def checkInterrupted(message: String = "") = {
    if (Thread.currentThread().isInterrupted()) throw InterruptedNotification(message)
  }

  def runWithLimit[A](limit: Long)(proc: => A): (Option[A], Long) = {
    var result = (None: Option[A], limit)

    val thread = new Thread {
      override def run() {
        val start = System.currentTimeMillis()
        val a = try {
          Some(proc)
        } catch {
          case e: InterruptedNotification =>
            Debug.debug(s"interrupted: ${e.message}")
            None
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
      Await.result(future, Duration(limit, MILLISECONDS))
    } catch {
      case _: TimeoutException =>
        thread.interrupt()
        thread.join()
        result
    }
  }
}
