package examples

import scala.language.implicitConversions

object Example07 extends App {

  case class RequestContext(requestId: String)

  class Logger {
    def log(message: String)(implicit ctx: RequestContext): Unit = {
      println(s"[${ctx.requestId}] $message")
    }
  }

  val logger: Logger = new Logger()

  def handle(implicit requestContext: RequestContext): Unit = {
    logger.log("Starting process")
    // some action ...
    logger.log("Continue process...")
    // some action ...
    logger.log("End process")
  }

  println(handle(RequestContext("ID")))
}
