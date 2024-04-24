package ex1.twitter

import ex1.twitter.domain.TwitterError.{LikeAlreadyExistError, LikeNotExistError, TweetNotExistError}
import ex1.twitter.domain.{TweetId, TweetInfo, TwitterError, User}

import java.time.Instant
import scala.collection.concurrent.TrieMap
import scala.util.{Failure, Success, Try}

trait TwitterApi {
  def tweet(user: User, text: String)(cb: Try[TweetId] => Unit): Unit

  def like(user: User, tweetId: TweetId)(cb: Try[Unit] => Unit): Unit

  def unlike(user: User, tweetId: TweetId)(cb: Try[Unit] => Unit): Unit

  def get(tweetId: TweetId)(cb: Try[TweetInfo] => Unit): Unit
}

/**
  * Представим, что это java реализация либы с очень грязным кодом.
  * Так делать НЕ НАДО!
  */
@SuppressWarnings(Array("Disable.ScalaCollection"))
class LocalTwitterApi(delays: Iterator[Int], knownDelays: Map[User, Int] = Map.empty) extends TwitterApi {
  private val tweets: TrieMap[TweetId, TweetInfo] = TrieMap.empty

  def tweet(user: User, text: String)(cb: Try[TweetId] => Unit): Unit = {
    val res =
      for {
        _       <- connectionFactor(Some(user))
        tweetId <- Success(TweetId.generate())
        created <- Success(Instant.now())
        _ = tweets += (tweetId -> TweetInfo(tweetId, created, text, user, Set.empty))
      } yield tweetId

    cb(res)
  }

  def like(user: User, tweetId: TweetId)(cb: Try[Unit] => Unit): Unit = {
    val res =
      connectionFactor(Some(user)).flatMap(_ =>
        this.synchronized {
          for {
            tweetInfo <- tryGet(tweetId)
            _         <- errorOn(tweetInfo.likedBy.contains(user))(LikeAlreadyExistError)
          } yield tweets.update(tweetInfo.id, tweetInfo.copy(likedBy = tweetInfo.likedBy + user))
        }
      )
    cb(res)
  }

  def unlike(user: User, tweetId: TweetId)(cb: Try[Unit] => Unit): Unit = {
    val res =
      connectionFactor(Some(user)).flatMap(_ =>
        this.synchronized {
          for {
            tweetInfo <- tryGet(tweetId)
            _         <- errorOn(!tweetInfo.likedBy.contains(user))(LikeNotExistError)
          } yield tweets.update(tweetInfo.id, tweetInfo.copy(likedBy = tweetInfo.likedBy - user))
        }
      )
    cb(res)
  }

  def get(tweetId: TweetId)(cb: Try[TweetInfo] => Unit): Unit = {
    val res =
      for {
        _         <- connectionFactor(None)
        tweetInfo <- tryGet(tweetId)
      } yield tweetInfo

    cb(res)
  }

  private def tryGet(tweetId: TweetId): Try[TweetInfo] =
    tweets.get(tweetId) match {
      case Some(tweetInfo) => Success(tweetInfo)
      case None            => Failure(TweetNotExistError)
    }

  private def errorOn(cond: Boolean)(err: TwitterError): Try[Unit] =
    if (cond) Failure(err) else Success()

  private def connectionFactor(user: Option[User]): Try[Unit] = {
    Thread.sleep(user.flatMap(knownDelays.get).getOrElse(delays.next()).toLong)
    Success(())
  }
}
