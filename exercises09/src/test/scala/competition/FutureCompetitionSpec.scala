package competition

import java.util.concurrent.Executors

import org.scalatest.wordspec.AnyWordSpec
import competition.domain.ScenarioError.TopAuthorNotFound
import service.{TwitterService, TwitterServiceFuture}
import twitter.domain.User
import twitter.{LocalTwitterApi, TwitterApi}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

class FutureCompetitionSpec extends AnyWordSpec {
  val oleg: User = User("oleg")
  val ivan: User = User("ivan")
  val bot: User  = User("bot")

  val users = List(oleg, ivan, bot)

  "FutureCompetition" should {
    "empty competition" in {
      implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(2))

      val api: TwitterApi                     = new LocalTwitterApi(Iterator.continually(0))
      val service: TwitterService[Future]     = new TwitterServiceFuture(api)
      val methods: CompetitionMethods[Future] = new CompetitionMethods[Future](service)

      val followers: Map[User, List[User]] = Map(
        oleg -> List(ivan),
        ivan -> List(oleg),
        bot  -> Nil
      )

      assert(
        Await
          .result(
            new FutureCompetition(service, methods)
              .winner(Nil, followers, bot)
              .map(Some(_))
              .recover {
                case TopAuthorNotFound => None
              },
            1.seconds
          )
          .isEmpty
      )
    }

    "async competition oleg win" in {
      implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(2))

      val api: TwitterApi                     = new LocalTwitterApi(Iterator.continually(0), Map(oleg -> 0, ivan -> 100))
      val service: TwitterService[Future]     = new TwitterServiceFuture(api)
      val methods: CompetitionMethods[Future] = new CompetitionMethods[Future](service)

      val followers: Map[User, List[User]] = Map(
        oleg -> List(ivan),
        ivan -> List(oleg),
        bot  -> Nil
      )

      assert(
        Await
          .result(new FutureCompetition(service, methods).winner(users, followers, bot), 1.seconds) == oleg
      )
    }

    "async competition ivan win first tweet" in {
      implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(2))

      val api: TwitterApi =
        new LocalTwitterApi(Iterator.continually(10), Map(ivan -> 0))
      val service: TwitterService[Future]     = new TwitterServiceFuture(api)
      val methods: CompetitionMethods[Future] = new CompetitionMethods[Future](service)

      val followers: Map[User, List[User]] = Map(
        oleg -> List(ivan),
        ivan -> List(oleg),
        bot  -> Nil
      )

      assert(
        Await
          .result(new FutureCompetition(service, methods).winner(users, followers, bot), 1.seconds) == ivan
      )
    }

    "sync competition oleg win" in {
      implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(1))

      val api: TwitterApi =
        new LocalTwitterApi(Iterator.continually(0), Map(oleg -> 100))
      val service: TwitterService[Future]     = new TwitterServiceFuture(api)
      val methods: CompetitionMethods[Future] = new CompetitionMethods[Future](service)

      val followers: Map[User, List[User]] = Map(
        oleg -> List(ivan),
        ivan -> List(oleg),
        bot  -> Nil
      )

      assert(
        Await
          .result(new FutureCompetition(service, methods).winner(users, followers, bot), 1.seconds) == oleg
      )
    }

    "async competition ivan win max likes" in {
      implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(2))

      val api: TwitterApi =
        new LocalTwitterApi(Iterator.continually(0))
      val service: TwitterService[Future]     = new TwitterServiceFuture(api)
      val methods: CompetitionMethods[Future] = new CompetitionMethods[Future](service)

      val followers: Map[User, List[User]] = Map(
        oleg -> List(ivan),
        ivan -> List(oleg, ivan),
        bot  -> Nil
      )

      assert(
        Await
          .result(new FutureCompetition(service, methods).winner(users, followers, bot), 1.seconds) == ivan
      )
    }

    "async competition ivan win bot removed" in {
      implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(2))

      val api: TwitterApi =
        new LocalTwitterApi(Iterator.continually(0), Map(oleg -> 100))
      val service: TwitterService[Future]     = new TwitterServiceFuture(api)
      val methods: CompetitionMethods[Future] = new CompetitionMethods[Future](service)

      val followers: Map[User, List[User]] = Map(
        oleg -> List(ivan, oleg, bot),
        ivan -> List(oleg, ivan),
        bot  -> Nil
      )

      assert(
        Await
          .result(new FutureCompetition(service, methods).winner(users, followers, bot), 1.seconds) == ivan
      )
    }
  }
}
