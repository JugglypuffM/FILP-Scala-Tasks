package service

import service.domain.GetTweetResponse.{Found, NotFound}
import service.domain.{GetTweetResponse, GetTweetsResponse}
import twitter.TwitterApi
import twitter.domain.TwitterError.{LikeAlreadyExistError, LikeNotExistError}
import twitter.domain.{TweetId, TweetInfo, User}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Success

/*
 * Future обертка над колбечным апи (TwitterAPI).
 */
class TwitterServiceFuture(api: TwitterApi)(implicit ec: ExecutionContext) extends TwitterService[Future] {
  def tweet(user: User, text: String): Future[TweetId] = {
    val promise: Promise[TweetId] = Promise[TweetId]
    Future(api.tweet(user, text)(promise.complete))
    promise.future
  }

  def like(user: User, tweetId: TweetId): Future[Unit] = {
    val promise: Promise[Unit] = Promise[Unit]
    Future(api.like(user, tweetId)(promise.complete))
    promise.future.recover({
      case LikeAlreadyExistError => ()
    })
  }

  def unlike(user: User, tweetId: TweetId): Future[Unit] = {
    val promise: Promise[Unit] = Promise[Unit]
    Future(api.unlike(user, tweetId)(promise.complete))
    promise.future
    promise.future.recover({
      case LikeNotExistError => ()
    })
  }

  def getTweet(tweetId: TweetId): Future[GetTweetResponse] = {
    val promise: Promise[GetTweetResponse] = Promise[GetTweetResponse]
    Future(
      api.get(tweetId)(result => promise.complete(Success(result.fold(_ => NotFound(tweetId), suc => Found(suc)))))
    )
    promise.future
  }

  def getTweets(ids: List[TweetId]): Future[GetTweetsResponse] = {
    Future
      .traverse(ids)(getTweet)
      .map(
        _.foldLeft(GetTweetsResponse(Set[TweetId](), Set[TweetInfo]()))((tweets, response) =>
          response match {
            case Found(v)    => GetTweetsResponse(tweets.notFound, tweets.found + v)
            case NotFound(v) => GetTweetsResponse(tweets.notFound + v, tweets.found)
          }
        )
      )
  }
}
