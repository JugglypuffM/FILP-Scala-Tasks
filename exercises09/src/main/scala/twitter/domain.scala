package twitter

import java.time.Instant

object domain {
  sealed trait TwitterError extends Throwable
  object TwitterError {
    case object LikeAlreadyExistError extends TwitterError
    case object LikeNotExistError     extends TwitterError
    case object TweetNotExistError    extends TwitterError
  }

  case class User(id: String)

  type TweetId = java.util.UUID
  object TweetId {
    def generate(): TweetId =
      java.util.UUID.randomUUID()
  }

  case class TweetInfo(id: TweetId, created: Instant, text: String, author: User, likedBy: Set[User])

}
