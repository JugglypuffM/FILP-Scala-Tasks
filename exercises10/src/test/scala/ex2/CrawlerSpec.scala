package ex2

import cats.effect.Ref
import ex2.HttpClient.{HttpResponse, URL}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers._

import scala.concurrent.ExecutionContext

class CrawlerSpec extends AnyWordSpec {
  val supabankRoot: URL = URL("http://supabank.org")
  val index: URL        = URL(supabankRoot.url + "/index.html")
  val about: URL        = URL(supabankRoot.url + "/about")
  val poweredBy: URL    = URL(supabankRoot.url + "/about/poweredby")
  val search: URL       = URL(supabankRoot.url + "/search")
  val api: URL          = URL(supabankRoot.url + "/api")
  val admin: URL        = URL(supabankRoot.url + "/admin.php")
  val analytics: URL    = URL(supabankRoot.url + "/admin/analytics")

  val wholeSite: Set[URL] = Set(supabankRoot, index, about, poweredBy, search, api, admin, analytics)

  trait Counter[F[_]] {
    def count: F[Map[URL, Int]]
  }

  private def mkClient(urls: Map[URL, String]): HttpClient[IO] with Counter[IO] =
    new HttpClient[IO] with Counter[IO] {
      val ref: Ref[IO, Map[URL, Int]] = Ref.unsafe(Map.empty)

      def get(url: URL): IO[HttpResponse] =
        for {
          _ <- ref.update(map => map.updated(url, map.getOrElse(url, 0) + 1))
          res <- urls.get(url) match {
            case Some(html) => IO.pure(HttpResponse(html))
            case None       => IO.raiseError(new RuntimeException("Url not found"))
          }
        } yield res

      val count: IO[Map[URL, Int]] =
        ref.get
    }

  "Crawler" should {
    "handle relative links" in {
      val client: HttpClient[IO] with Counter[IO] = mkClient(
        Map(
          supabankRoot -> """
          <html><body>
            <a href="/index.html">Home</a>
            <a href="/search">Search</a>
            <a href="/about">About</a>
            <a href="/admin.php">Adminka</a>
            <a href="/api">Api</a>
          </body></html>
          """,
          search       -> """
                <html><body>
                  <a href="/index.html">Home</a>
                  <a href="/search">Search</a>
                  <a href="/about">About</a>
                </body></html>
                """,
          index        -> """
                <html><body>
                  <a href="/index.html">Home</a>
                  <a href="/search">Search</a>
                  <a href="/about">About</a>
                </body></html>
                """,
          about        -> """
                <html><body>
                  <a href="/index.html">Home</a>
                  <a href="/search">Search</a>
                  <a href="/about">About</a>
                  <a href="/about/poweredby">Powered By</a>
                </body></html>
                """,
          poweredBy    -> """
                <html><body>
                  <a href="/index.html">Home</a>
                  <a href="/search">Search</a>
                  <a href="/about">About</a>
                  <a href="/about/poweredby">Powered By</a>
                </body></html>
                """,
          admin        -> """
                <html><body>
                  <a href="/admin/analytics">Powered By</a>
                </body></html>
                """,
          analytics    -> """<html><body></body></html>""",
          api          -> """<html><body></body></html>"""
        )
      )

      val crawler = new Crawler(client)

      val assertation =
        for {
          result <- crawler.crawl(supabankRoot)
          count  <- client.count
        } yield {
          count.foreach {
            case (url, count) =>
              withClue(s"Number of '${url.url}' invocations should be 1") {
                count shouldBe 1
              }
          }
          result should contain theSameElementsAs wholeSite
        }

      assertation.unsafeRunSync()
    }

    "handle absolute links" in {
      val client: HttpClient[IO] with Counter[IO] = mkClient(
        Map(
          supabankRoot -> """
          <html><body>
            <a href="http://supabank.org/index.html">Home</a>
            <a href="http://supabank.org/search">Search</a>
            <a href="http://supabank.org/about">About</a>
            <a href="http://supabank.org/admin.php">Adminka</a>
            <a href="http://supabank.org/api">Api</a>
          </body></html>
          """,
          search       -> """
                <html><body>
                  <a href="/index.html">Home</a>
                  <a href="/search">Search</a>
                  <a href="/about">About</a>
                </body></html>
                """,
          index        -> """
                <html><body>
                  <a href="/index.html">Home</a>
                  <a href="/search">Search</a>
                  <a href="/about">About</a>
                </body></html>
                """,
          about        -> """
                <html><body>
                  <a href="/index.html">Home</a>
                  <a href="/search">Search</a>
                  <a href="/about">About</a>
                  <a href="/about/poweredby">Powered By</a>
                </body></html>
                """,
          poweredBy    -> """
                <html><body>
                  <a href="/index.html">Home</a>
                  <a href="/search">Search</a>
                  <a href="/about">About</a>
                  <a href="/about/poweredby">Powered By</a>
                </body></html>
                """,
          admin        -> """
                <html><body>
                  <a href="/admin/analytics">Powered By</a>
                </body></html>
                """,
          analytics    -> """<html><body></body></html>""",
          api          -> """<html><body></body></html>"""
        )
      )

      val crawler = new Crawler(client)

      val assertation =
        for {
          result <- crawler.crawl(supabankRoot)
          count  <- client.count
        } yield {
          count.foreach {
            case (url, count) =>
              withClue(s"Number of '${url.url}' invocations should be 1") {
                count shouldBe 1
              }
          }
          result should contain theSameElementsAs wholeSite
        }

      assertation.unsafeRunSync()
    }

    "ignore external links" in {
      val client: HttpClient[IO] with Counter[IO] =
        mkClient(Map(supabankRoot -> """
          <html><body>
            <a href="http://supabank-ext.org/index.html">Home</a>
            <a href="http://supabank-ext.org/search">Search</a>
            <a href="http://supabank-ext.org/about">About</a>
            <a href="http://supabank-ext.org/admin.php">Adminka</a>
            <a href="http://supabank-ext.org/api">Api</a>
          </body></html>
          """))

      val crawler = new Crawler(client)

      val assertation =
        for {
          result <- crawler.crawl(supabankRoot)
          count  <- client.count
        } yield {
          assert(result == Set(supabankRoot))
          assert(count == Map(supabankRoot -> 1))
        }

      assertation.unsafeRunSync()
    }

    "handle empty page" in {
      val client: HttpClient[IO] with Counter[IO] =
        mkClient(Map(supabankRoot -> """
          <html><body>
          </body></html>
          """))

      val crawler = new Crawler(client)

      val assertation =
        for {
          result <- crawler.crawl(supabankRoot)
          count  <- client.count
        } yield {
          assert(result == Set(supabankRoot))
          assert(count == Map(supabankRoot -> 1))
        }

      assertation.unsafeRunSync()
    }

    "ignore bad urls after 3 tries" in {
      val client: HttpClient[IO] with Counter[IO] =
        mkClient(Map(supabankRoot -> """
          <html><body>
          <a href="http://supabank.org/index.html">Home</a>
          <a href="/search">Search</a>
          </body></html>
          """))

      val crawler = new Crawler(client)

      val assertation =
        for {
          result <- crawler.crawl(supabankRoot)
          count  <- client.count
        } yield {
          assert(result == Set(supabankRoot))
          assert(count == Map(supabankRoot -> 1, index -> 3, search -> 3))
        }

      assertation.unsafeRunSync()
    }

    "handle start from not / url" in {
      val baseUrl = URL("http://supabank.org/lol/kek.php")

      val client: HttpClient[IO] with Counter[IO] =
        mkClient(
          Map(
            baseUrl -> """
          <html><body>
          <a href="http://supabank.org/index.html">Home</a>
          </body></html>
          """,
            index   -> """
          <html><body>
          </body></html>
          """
          )
        )

      val crawler = new Crawler(client)

      val assertation =
        for {
          result <- crawler.crawl(baseUrl)
          count  <- client.count
        } yield {
          assert(result == Set(baseUrl, index))
          assert(count == Map(baseUrl -> 1, index -> 1))
        }

      assertation.unsafeRunSync()
    }

    "handle ../ links" in {
      val baseUrl = URL("http://supabank.org/lol/kek.php")

      val client: HttpClient[IO] with Counter[IO] =
        mkClient(
          Map(
            baseUrl -> """
          <html><body>
          <a href="http://supabank.org/index.html">Home</a>
          <a href="../search">Search</a>
          </body></html>
          """,
            index   -> """
          <html><body>
          </body></html>
          """,
            search  -> """
          <html><body>
          </body></html>
          """
          )
        )

      val crawler = new Crawler(client)

      val assertation =
        for {
          result <- crawler.crawl(baseUrl)
          count  <- client.count
        } yield {
          assert(result == Set(baseUrl, index, search))
          assert(count == Map(baseUrl -> 1, index -> 1, search -> 1))
        }

      assertation.unsafeRunSync()
    }

    "handle success after 3 failures" in {
      val client: HttpClient[IO] = new HttpClient[IO] {
        val counter: Ref[IO, Int] = Ref.unsafe(0)

        def get(url: URL): IO[HttpResponse] =
          for {
            count <- counter.get
            res <- if (count < 2)
              counter.set(count + 1) *> IO.raiseError(new RuntimeException("Connection error"))
            else IO.pure(HttpResponse(""))
          } yield res
      }

      val crawler = new Crawler(client)

      val assertation =
        for {
          result <- crawler.crawl(supabankRoot)
        } yield {
          assert(result == Set(supabankRoot))
        }

      assertation.unsafeRunSync()
    }
  }
}
