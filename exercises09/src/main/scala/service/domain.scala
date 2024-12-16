package service

import twitter.domain.{TweetId, TweetInfo}

object domain {
  sealed trait GetTweetResponse
  object GetTweetResponse {
    case class NotFound(tweetId: TweetId) extends GetTweetResponse
    case class Found(info: TweetInfo)     extends GetTweetResponse

    // Чтобы не делать апкасты
    def found(info: TweetInfo): GetTweetResponse =
      Found(info)

    def notFound(tweetId: TweetId): GetTweetResponse =
      NotFound(tweetId)
  }

  case class GetTweetsResponse(notFound: Set[TweetId], found: Set[TweetInfo])
}
