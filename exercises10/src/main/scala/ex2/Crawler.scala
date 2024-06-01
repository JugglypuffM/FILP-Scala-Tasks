package ex2

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._

import scala.annotation.tailrec

/**
  * В данной работе необходимо написать web-crawler.
  *
  * Crawler - это программа, которая анализует структуру данного ей сайта.
  * Эта программа посещает все ссылки, которые ей удаётся найти на заданном ей ресурсе.
  *
  * Вам необходимо реализовать функцию `crawl`.
  *
  * Эта функция будет вызвана с некоторым урлом. Например: "http://wikipedia.org"
  * Задача этой функции:
  *   1. Посетить переданный ей урл.
  *   2. Обработать ответ формата html - получив новые урлы для посещения.
  *   3. Посетить полученные из ответа урлы, в случае если они принадлежат тому же домену (поддомены не считать, только строгое совпадение).
  *   4. Для каждого урла, из шага 3, повторять все с 1 шага до тех пор, пока Crawler не посетит все страницы
  *   5. Вернуть набор посещённых страниц
  *
  * Таким образом, программа должна посетить каждую страницу сайта, до которой сможет добраться через html ответы.
  *
  * При этом, должны соблюдаться условия:
  *   - Каждая страница должна быть посещена строго 1 раз.
  *   - Ответ HttpClient может вернуть ошибку (завершение IO с Exception). В этом случае, необходимо попытаться
  *     повторить запрос, но не более 3 раз. Если после 3 запросов всё ещё возвращается ошибка,
  *     урл необходимо пропустить (урла не должно быть в результате функции).
  *   - Нельзя покидать сайт. Скажем если на сайте "http://wikipedia.org" вы вдруг найдёт ссылку на сторонний ресурс,
  *     её необходимо пропустить (её нельзя вызывать, урла не должно быть в результате функции).
  *
  * Прочие условности:
  *   - Пример html ответа
  *       <html><body>
  *         <div>some hmtl here</div>
  *         <div>quite <p style="color: red">natural</p></div>
  *         <div><a href="/page1">first page</a></div>
  *       </body></html>
  *     Таким образом, следует считать что html ответ приближен к реальности.
  *   - Искомые ссылки находятся в атрибуте `href`
  *   - На странице может быть 0+ ссылок. Не обязательно одна.
  *   - Поиск по html можете производить любым удобным способом.
  *     Например, можете использовать UrlSearch.search
  *   - Учитывайте, что URL адреса могут быть как абсолютными так и относительными.
  *   - В идеале, обработку ссылок выполнять конкурентно, использовав параллельные комбинаторы.
  *   - Обычно, в реальном мире, перед тем как совершить повторный запрос, вы бы выжидали какое-то время.
  *     Возможно даже это время увеличивалось бы с каждый запросом согласно какой-то функции.
  *     Однако в данной работе, не нужно выжидать и делать паузы. Именно по этой причине вам не предоставлен Timer.
  *     Получив ошибку, необходимо сразу же повторить запрос.
  *   - Можете определять доменную модель, как считаете нужным.
  */
class Crawler(client: HttpClient[IO]) {

  private val ATTEMPTS_LEFT: Int = 2

  def crawl(root: HttpClient.URL): IO[Set[HttpClient.URL]] = {

    def tryVisit(url: HttpClient.URL, attemptsLeft: Int): IO[Either[HttpClient.URL, Set[HttpClient.URL]]] =
      for {
        response <- client.get(url).attempt
        result <- response match {
          case Left(_) if attemptsLeft != 0 => tryVisit(url, attemptsLeft - 1)
          case Left(_)                      => IO.pure(Left(url))
          case Right(html)                  => IO.pure(Right(UrlSearch.search(root, url, html)))
        }
      } yield result

    def mapResults(
        results: List[Either[HttpClient.URL, Set[HttpClient.URL]]]
    ): (Set[HttpClient.URL], Set[HttpClient.URL]) =
      results.partitionMap(identity).bimap(_.toSet, _.foldLeft(Set.empty[HttpClient.URL])(_ |+| _))

    def inner(
        unvisited: Set[HttpClient.URL],
        visited: Set[HttpClient.URL],
        banned: Set[HttpClient.URL]
    ): IO[Set[HttpClient.URL]] =
      for {
        results <- unvisited.toList.parTraverse(tryVisit(_, ATTEMPTS_LEFT))
        (justBanned, justFound) = mapResults(results)
        newBanned               = banned |+| justBanned
        newVisited              = visited |+| unvisited.diff(justBanned)
        newUnvisited            = justFound.diff(newVisited |+| newBanned)
        result <- if (newUnvisited.nonEmpty) {
          inner(newUnvisited, newVisited, newBanned)
        } else {
          IO.pure(newVisited)
        }
      } yield result

    inner(Set(root), Set(), Set())
  }
}
