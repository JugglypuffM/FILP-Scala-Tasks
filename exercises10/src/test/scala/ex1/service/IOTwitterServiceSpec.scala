package ex1.service

import cats.effect._
import cats.effect.testing.scalatest.AsyncIOSpec
import ex1.service.{TwitterService, TwitterServiceIO}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import ex1.service.domain.GetTweetResponse.{Found, NotFound}
import ex1.twitter.{LocalTwitterApi, TwitterApi}
import ex1.twitter.domain.{TweetId, TweetInfo, User}

class IOTwitterServiceSpec extends AsyncWordSpec with AsyncIOSpec with Matchers {
  "IOTwitterService" should {
    "tweet and get" in {

      val api: TwitterApi             = new LocalTwitterApi(Iterator.continually(0))
      val service: TwitterService[IO] = new TwitterServiceIO(api)

      val res = for {
        id    <- service.tweet(User("abc"), "abc")
        tweet <- service.getTweet(id)
      } yield (tweet, id)

      res.asserting {
        _ should matchPattern {
          case (Found(TweetInfo(idR, _, _, _, _)), id) if idR == id => ()
        }
      }
    }

    "tweet two and get two" in {
      val api: TwitterApi             = new LocalTwitterApi(Iterator.continually(0))
      val service: TwitterService[IO] = new TwitterServiceIO(api)

      val res = for {
        id    <- service.tweet(User("abc"), "abc")
        id2   <- service.tweet(User("abc"), "abc")
        tweet <- service.getTweets(List(id, id2))
      } yield (tweet, id, id2)

      res.asserting {
        case (response, id1, id2) =>
          response.found.map(_.id) shouldBe Set(id1, id2)
      }
    }

    "not found" in {
      val api: TwitterApi             = new LocalTwitterApi(Iterator.continually(0))
      val service: TwitterService[IO] = new TwitterServiceIO(api)

      val notFoundId = TweetId.generate()
      val response   = service.getTweet(notFoundId)

      response.asserting(_ should matchPattern {
        case NotFound(`notFoundId`) => ()
      })
    }

    "tweet and like" in {
      val api: TwitterApi             = new LocalTwitterApi(Iterator.continually(0))
      val service: TwitterService[IO] = new TwitterServiceIO(api)

      val user = User("abc")
      val response = for {
        id    <- service.tweet(user, "abc")
        _     <- service.like(user, id)
        tweet <- service.getTweet(id)
      } yield tweet

      response.asserting(_ should matchPattern {
        case Found(info) if info.likedBy == Set(user) => ()
      })
    }

    "two like idempotent" in {
      val api: TwitterApi             = new LocalTwitterApi(Iterator.continually(0))
      val service: TwitterService[IO] = new TwitterServiceIO(api)

      val user = User("abc")
      val response = for {
        id    <- service.tweet(user, "abc")
        _     <- service.like(user, id)
        _     <- service.like(user, id)
        tweet <- service.getTweet(id)
      } yield tweet

      response.asserting(_ should matchPattern {
        case Found(info) if info.likedBy == Set(user) => ()
      })
    }

    "tweet, like, unlike" in {
      val api: TwitterApi             = new LocalTwitterApi(Iterator.continually(0))
      val service: TwitterService[IO] = new TwitterServiceIO(api)

      val user = User("abc")
      val responses = for {
        id           <- service.tweet(user, "abc")
        _            <- service.like(user, id)
        likedTweet   <- service.getTweet(id)
        _            <- service.unlike(user, id)
        unlikedTweet <- service.getTweet(id)
      } yield (likedTweet, unlikedTweet)

      responses.asserting(_ should matchPattern {
        case (Found(info1), Found(info2)) if info1.likedBy == Set(user) && info2.likedBy.isEmpty => ()
      })
    }

    "two unlike idempotent" in {
      val api: TwitterApi             = new LocalTwitterApi(Iterator.continually(0))
      val service: TwitterService[IO] = new TwitterServiceIO(api)

      val user = User("abc")
      val responses = for {
        id           <- service.tweet(user, "abc")
        _            <- service.like(user, id)
        likedTweet   <- service.getTweet(id)
        _            <- service.unlike(user, id)
        _            <- service.unlike(user, id)
        unlikedTweet <- service.getTweet(id)
      } yield (likedTweet, unlikedTweet)

      responses.asserting(_ should matchPattern {
        case (Found(info1), Found(info2)) if info1.likedBy == Set(user) && info2.likedBy.isEmpty => ()
      })
    }
  }
}
