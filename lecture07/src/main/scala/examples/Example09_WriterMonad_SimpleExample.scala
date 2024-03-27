package examples

import examples.data.Writer
import examples.typeclasses.MonadSyntax.{MonadOps, PureOps}
import examples.typeclasses.MonoidInstances._

object Example09_WriterMonad_SimpleExample extends App {

  type Logs = List[String]

  def httpRequest(url: String): Writer[Logs, String] =
    for {
      _        <- Writer.tell[Logs](List(s"Http request $url"))
      response <- "Bob".pure[Writer[Logs, *]]
    } yield response

  def getUserName(id: Int): Writer[Logs, String] =
    for {
      name <- httpRequest(s"http://some-service/$id")
      _    <- Writer.tell[Logs](List(s"Fetched name $name"))
    } yield name

  val (logs, result) = getUserName(42).run

  println(logs.mkString("\n"))
  println()
  println(result)
}
