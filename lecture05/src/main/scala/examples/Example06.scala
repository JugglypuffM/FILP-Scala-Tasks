package examples

import scala.language.implicitConversions

object Example06 extends App {

  case class RequestContext(requestId: String)

  class Logger {
    def log(message: String)(ctx: RequestContext): Unit = {
      println(s"[${ctx.requestId}] $message")
    }
  }

  val logger: Logger = new Logger()

  def handle(requestContext: RequestContext): Unit = {
    logger.log("Starting process")(requestContext)
    // some action ...
    logger.log("Continue process...")(requestContext)
    // some action ...
    logger.log("End process")(requestContext)
  }

  println(handle(RequestContext("ID")))
}
