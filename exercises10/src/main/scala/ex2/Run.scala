package ex2

import cats.effect.kernel.Resource
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import ex2.HttpClient.{HttpResponse, URL}

object Run extends IOApp {

  val httpClient: HttpClient[IO] = new HttpClient[IO] {
    override def get(url: URL): IO[HttpResponse] =
      IO.bracketFull(_ => IO(scala.io.Source.fromURL(url.url)))(source => HttpResponse(source.mkString).pure[IO])(
        (source, _) => IO(source.close())
      )
  }

  val crawler: Crawler = new Crawler(httpClient)

  def run(args: List[String]): IO[ExitCode] =
    for {
      res <- crawler.crawl(URL("http://filp.ulanzetz.com/exercises10"))
      _   <- IO.delay(println(res))
    } yield ExitCode.Success
}
