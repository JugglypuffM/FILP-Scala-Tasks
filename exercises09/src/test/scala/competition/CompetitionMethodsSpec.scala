package competition

import cats.effect._
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import service.domain.GetTweetResponse.Found
import service.TwitterService
import twitter.domain.User
import twitter.{LocalTwitterApi, TwitterApi}

class CompetitionMethodsSpec extends AsyncWordSpec with AsyncIOSpec with Matchers {
  "CompetitionMethods" should {
    "unlikeAll one tweet" in {

      val api: TwitterApi                 = new LocalTwitterApi(Iterator.continually(0))
      val service: TwitterService[IO]     = new TwitterServiceIO(api)
      val methods: CompetitionMethods[IO] = new CompetitionMethods[IO](service)

      val user = User("abc")
      val res = for {
          id           <- service.tweet(user, "abc")
          _            <- service.like(user, id)
          likedTweet   <- service.getTweet(id)
          _            <- methods.unlikeAll(user, List(id))
          unlikedTweet <- service.getTweet(id)
        } yield (likedTweet, unlikedTweet)

      res.asserting { res => res should matchPattern {
        case (Found(info1), Found(info2)) if
          info1.likedBy == Set(user) && info2.likedBy.isEmpty => ()
        }
      }
    }

    "unlikeAll tweets" in {

      val api: TwitterApi                 = new LocalTwitterApi(Iterator.continually(0))
      val service: TwitterService[IO]     = new TwitterServiceIO(api)
      val methods: CompetitionMethods[IO] = new CompetitionMethods[IO](service)

      val user = User("abc")
      val res = for {
          id            <- service.tweet(user, "abc")
          id2           <- service.tweet(user, "abc2")
          _             <- service.like(user, id)
          _             <- service.like(user, id2)
          _             <- methods.unlikeAll(user, List(id, id2))
          unlikedTweet  <- service.getTweet(id)
          unlikedTweet2 <- service.getTweet(id2)
        } yield (unlikedTweet, unlikedTweet2)

      res.asserting(r => r should matchPattern {
        case (Found(info1), Found(info2)) if
          info1.likedBy.isEmpty && info2.likedBy.isEmpty => ()
      })
    }

    "unlikeAll no tweets" in {

      val api: TwitterApi                 = new LocalTwitterApi(Iterator.continually(0))
      val service: TwitterService[IO]     = new TwitterServiceIO(api)
      val methods: CompetitionMethods[IO] = new CompetitionMethods[IO](service)

      val user = User("abc")
      val res = for {
          id           <- service.tweet(user, "abc")
          _            <- service.like(user, id)
          likedTweet   <- service.getTweet(id)
          _            <- methods.unlikeAll(user, List())
          unlikedTweet <- service.getTweet(id)
        } yield (likedTweet, unlikedTweet)

      res.asserting {
        case (response1, response2) => response1 shouldBe response2
      }
    }

    "topAuthor by likes" in {

      val api: TwitterApi                 = new LocalTwitterApi(Iterator.continually(0))
      val service: TwitterService[IO]     = new TwitterServiceIO(api)
      val methods: CompetitionMethods[IO] = new CompetitionMethods[IO](service)

      val user  = User("abc")
      val user2 = User("abc2")
      val topAuthor = for {
          id        <- service.tweet(user, "abc")
          id2       <- service.tweet(user2, "abc2")
          _         <- service.like(user, id)
          _         <- service.like(user, id2)
          _         <- service.like(user2, id2)
          topAuthor <- methods.topAuthor(List(id, id2))
        } yield topAuthor

      topAuthor.asserting(_ shouldBe Some(user2))
    }

    "topAuthor by time" in {

      val api: TwitterApi                 = new LocalTwitterApi(Iterator.continually(0))
      val service: TwitterService[IO]     = new TwitterServiceIO(api)
      val methods: CompetitionMethods[IO] = new CompetitionMethods[IO](service)

      val user  = User("abc")
      val user2 = User("abc2")
      val topAuthor = for {
          id        <- service.tweet(user, "abc")
          id2       <- service.tweet(user2, "abc2")
          _         <- service.like(user, id)
          _         <- service.like(user2, id2)
          topAuthor <- methods.topAuthor(List(id, id2))
        } yield topAuthor

      topAuthor.asserting(_ shouldBe Some(user))
    }
  }
}