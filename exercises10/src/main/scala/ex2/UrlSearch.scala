package ex2

import ex2.HttpClient.{HttpResponse, URL}

import java.net.{URL => JURL}
import scala.util.matching.Regex

object UrlSearch {
  val urlRegexp: Regex = "href=[\"\']([^\"\']+)[\"\']".r

  def search(root: URL, currentUrl: URL, response: HttpResponse): Set[URL] = {
    urlRegexp
      .findAllMatchIn(response.html)
      .map {
        case urlRegexp(url) => URL(url)
      }
      .toSet
      .map(url => normalize(currentUrl, url))
      .filter(url => sameDomain(root, url))
  }

  def normalize(currentUrl: URL, url: URL): URL = {
    URL(new JURL(new JURL(currentUrl.url), url.url).toExternalForm)
  }

  def sameDomain(root: HttpClient.URL, u: HttpClient.URL): Boolean =
    new JURL(root.url).getHost == new JURL(u.url).getHost

}
