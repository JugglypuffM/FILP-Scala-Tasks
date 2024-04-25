package competition

import cats.Monad
import cats.syntax.all._
import cats.instances.all._
import service.TwitterService
import twitter.domain.{TweetId, TweetInfo, User}

import scala.math.Ordered.orderingToOrdered

class CompetitionMethods[F[_]: Monad](service: TwitterService[F]) {

  /**
    * В этом методе надо:
    * Загрузить все указанные твиты
    * найти в них твиты где есть лайки указанного юзера
    * удалить эти лайки вызвав unlike
    */
  def unlikeAll(user: User, tweetIds: List[TweetId]): F[Unit] =
    for {
      tweets <- service.getTweets(tweetIds)
      result <- tweets.found
        .filter { case TweetInfo(_, _, _, _, likes) => likes.contains(user) }
        .toList
        .traverse(info => service.unlike(user, info.id))
    } yield result

  /**
    * В этом методе надо:
    * Загрузить все указанные твиты
    * выбрать среди них тот твит у которого больше всего лайков или он раньше создан, если лайков одинаковое количество
    */
  def topAuthor(tweetIds: List[TweetId]): F[Option[User]] = {
    for {
      tweets <- service.getTweets(tweetIds)
    } yield tweets.found
      .maxByOption(identity)((cur, next) => (cur.likedBy.size, next.created).compareTo(next.likedBy.size, cur.created))
      .map(x => x.author)
  }
}
