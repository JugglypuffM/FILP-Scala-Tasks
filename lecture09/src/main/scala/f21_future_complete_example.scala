import java.util.concurrent.{Executors, TimeUnit}
import scala.concurrent.{ExecutionContext, Future, Promise}

object f21_future_complete_example extends App {
  implicit val ec = ExecutionContext.global
  val scheduler = Executors.newScheduledThreadPool(1)


  case class Request(user: String, destination: String, message: String)

  def validateRequest(req: Request): Future[Unit] =
    if (req.user.isEmpty || req.message.isEmpty || req.destination.isEmpty) {
      Future.failed(new RuntimeException("Oops! Invalid request"))
    } else Future.unit

  def authorizeUser(user: String): Future[Unit] =
    if(user.startsWith("admin")) {
      Future.unit
    } else {
      Future.failed(new RuntimeException("User authorization failed"))
    }

  def sendInDatabaseData(user: String, message: String): Future[Boolean] = {
    val promise: Promise[Boolean] = Promise()
    scheduler.schedule(
      new Runnable {
        override def run(): Unit = if(message.nonEmpty) promise.success(true) else promise.success(false)
      },
      5, TimeUnit.SECONDS
    )
    // Sending in DB
    promise.future
  }

  def sendNotifications(user: String, message: String, destination: String): Future[Unit] = {
    val promise: Promise[Unit] = Promise()
    scheduler.schedule(
      new Runnable {
        override def run(): Unit = promise.success()
      },
      5, TimeUnit.SECONDS
    )
    // Sending in DB
    promise.future
  }


  def processRequest(request: Request, sendResponse: Unit => Future[Unit]): Future[Unit] =
    for {
      _ <- validateRequest(request)
      _ <- authorizeUser(request.user)
      result <- sendInDatabaseData(request.user, request.message)
      _ <- if(result) {
        Future.unit
      } else {
        Future.failed(new RuntimeException("Error from DB"))
      }
      _ <- sendNotifications(request.user, request.message, request.destination)
      _ <- sendResponse()
    } yield ()

}
