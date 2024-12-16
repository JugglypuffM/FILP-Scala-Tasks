package ex1.competition

import cats.effect._
import cats.effect.testing.scalatest.AsyncIOSpec
import ex1.competition.{CompetitionMethods, IOCompetition}
import ex1.competition.domain.ScenarioError.TopAuthorNotFound
import ex1.service.{TwitterService, TwitterServiceIO}
import ex1.twitter.{LocalTwitterApi, TwitterApi}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import ex1.twitter.domain.User

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext.fromExecutor

class IOCompetitionSpec extends AsyncWordSpec with AsyncIOSpec with Matchers {
  val oleg: User = User("oleg")
  val ivan: User = User("ivan")
  val igor: User = User("igor")
  val bot: User  = User("bot")

  val users = List(oleg, ivan, bot)

  val ctx1 = fromExecutor(Executors.newFixedThreadPool(1))
  val ctx2 = fromExecutor(Executors.newFixedThreadPool(2))

  "IOCompetition" should {
    "empty competition" in {
      val api: TwitterApi                 = new LocalTwitterApi(Iterator.continually(0))
      val service: TwitterService[IO]     = new TwitterServiceIO(api)
      val methods: CompetitionMethods[IO] = new CompetitionMethods[IO](service)

      val followers: Map[User, List[User]] = Map(
        oleg -> List(ivan),
        ivan -> List(oleg),
        bot  -> Nil
      )

      new IOCompetition(service, methods)
        .winner(Nil, followers, bot)
        .map(Some(_))
        .handleErrorWith { case TopAuthorNotFound => IO.pure(None) }
        .asserting(_ shouldBe None)
    }

    "async competition oleg win" in {
      val api: TwitterApi                 = new LocalTwitterApi(Iterator.continually(0), Map(oleg -> 0, ivan -> 100))
      val service: TwitterService[IO]     = new TwitterServiceIO(api)
      val methods: CompetitionMethods[IO] = new CompetitionMethods[IO](service)

      val followers: Map[User, List[User]] = Map(
        oleg -> List(ivan),
        ivan -> List(oleg),
        bot  -> Nil
      )

      new IOCompetition(service, methods)
        .winner(users, followers, bot)
        .evalOn(ctx2)
        .asserting(_ shouldBe oleg)
    }

    "async competition ivan win first tweet" in {
      val api: TwitterApi =
        new LocalTwitterApi(Iterator.continually(10), Map(ivan -> 0))
      val service: TwitterService[IO]     = new TwitterServiceIO(api)
      val methods: CompetitionMethods[IO] = new CompetitionMethods[IO](service)

      val followers: Map[User, List[User]] = Map(
        oleg -> List(ivan),
        ivan -> List(oleg),
        bot  -> Nil
      )

      new IOCompetition(service, methods)
        .winner(users, followers, bot)
        .evalOn(ctx2)
        .asserting(_ shouldBe ivan)
    }

    "sync competition oleg win" in {

      val api: TwitterApi =
        new LocalTwitterApi(Iterator.continually(0), Map(oleg -> 100))
      val service: TwitterService[IO]     = new TwitterServiceIO(api)
      val methods: CompetitionMethods[IO] = new CompetitionMethods[IO](service)

      val followers: Map[User, List[User]] = Map(
        oleg -> List(ivan),
        ivan -> List(oleg),
        bot  -> Nil
      )

      new IOCompetition(service, methods)
        .winner(users, followers, bot)
        .evalOn(ctx1)
        .asserting(_ shouldBe oleg)
    }

    "async competition ivan win max likes" in {
      val api: TwitterApi =
        new LocalTwitterApi(Iterator.continually(10))
      val service: TwitterService[IO]     = new TwitterServiceIO(api)
      val methods: CompetitionMethods[IO] = new CompetitionMethods[IO](service)

      val followers: Map[User, List[User]] = Map(
        oleg -> List(ivan),
        ivan -> List(oleg, igor),
        bot  -> Nil
      )

      new IOCompetition(service, methods)
        .winner(users, followers, bot)
        .evalOn(ctx2)
        .asserting(_ shouldBe ivan)
    }

    "async competition ivan win bot removed" in {
      val api: TwitterApi =
        new LocalTwitterApi(Iterator.continually(0), Map(oleg -> 100))
      val service: TwitterService[IO]     = new TwitterServiceIO(api)
      val methods: CompetitionMethods[IO] = new CompetitionMethods[IO](service)

      val followers: Map[User, List[User]] = Map(
        oleg -> List(ivan, oleg, bot),
        ivan -> List(oleg, ivan),
        bot  -> Nil
      )

      new IOCompetition(service, methods)
        .winner(users, followers, bot)
        .evalOn(ctx2)
        .asserting(_ shouldBe ivan)
    }
  }
}
