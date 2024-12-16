package ex1.service

import cats.effect.IO
import cats.syntax.all._
import ex1.service.domain.GetTweetResponse.{Found, NotFound}
import ex1.service.domain.{GetTweetResponse, GetTweetsResponse}
import ex1.twitter.TwitterApi
import ex1.twitter.domain.TwitterError.{LikeAlreadyExistError, LikeNotExistError, TweetNotExistError}
import ex1.twitter.domain._

import scala.util.{Failure, Success}

// Воспользуйтесь синтаксисом map, recover, traverse из cats.syntax.all_
class TwitterServiceIO(api: TwitterApi) extends TwitterService[IO] {
  def tweet(user: User, text: String): IO[TweetId] = IO.async_(cb => api.tweet(user, text)(x => cb(x.toEither)))

  def like(user: User, tweetId: TweetId): IO[Unit] =
    IO.async_((cb: Either[Throwable, Unit] => Unit) => api.like(user, tweetId)(x => cb(x.toEither))).recover {
      case LikeAlreadyExistError => Right()
    }

  def unlike(user: User, tweetId: TweetId): IO[Unit] =
    IO.async_((cb: Either[Throwable, Unit] => Unit) => api.unlike(user, tweetId)(x => cb(x.toEither))).recover {
      case LikeNotExistError => Right()
    }

  def getTweet(tweetId: TweetId): IO[GetTweetResponse] =
    IO.async_((cb: Either[Throwable, GetTweetResponse] => Unit) => api.get(tweetId)(x => cb(x.toEither.map(Found))))
      .recover {
        case TweetNotExistError => NotFound(tweetId)
      }

  def getTweets(ids: List[TweetId]): IO[GetTweetsResponse] =
    for {
      tweets <- ids.traverse(getTweet)
      res = tweets.partitionMap {
        case Found(info)  => Right(info)
        case NotFound(id) => Left(id)
      }
      found    = res._2.toSet
      notFound = res._1.toSet
    } yield GetTweetsResponse(notFound, found)
}
