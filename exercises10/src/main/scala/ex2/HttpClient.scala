package ex2

import HttpClient._

trait HttpClient[F[_]] {
  def get(url: URL): F[HttpResponse]
}

object HttpClient {
  case class URL(url: String)
  case class HttpResponse(html: String)
}
