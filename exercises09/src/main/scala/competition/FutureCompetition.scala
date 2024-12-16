package competition

import competition.domain.ScenarioError.TopAuthorNotFound
import service.TwitterService
import twitter.domain.User

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/**
  * Конкурс! Кто наберет больше лайков под своим постом - тот победил
  *
  * Каждый пользовать постит твит "${user.id} will win!", и его фолловеры его лайкают
  * юзеры постят твиты параллельно, и так же параллельно их лайкают фолловеры
  *
  * Но случилась беда: пользователь с именем bot нарушил правила конкурса, и все его лайки надо удалить
  *
  * В конце надо вывести победителя
  * Если победителей несколько, то того, у которого твит был раньше
  * Если победителей нет, то вернуть ошибку TopAuthorNotFound
  *
  * используйте методы
  * CompetitionMethods.unlikeAll
  * CompetitionMethods.topAuthor
  */
class FutureCompetition(service: TwitterService[Future], methods: CompetitionMethods[Future])(
    implicit ec: ExecutionContext
) extends Competition[Future] {
  def winner(
      users: List[User],
      followers: Map[User, List[User]],
      botUser: User
  ): Future[User] =
    for {
      tweets <- Future.traverse(users)(user =>
        service
          .tweet(user, s"${user.id} will win!")
          .flatMap(id => Future.traverse(followers(user))(service.like(_, id)).map(_ => id))
      )
      _            <- methods.unlikeAll(botUser, tweets)
      winnerOption <- methods.topAuthor(tweets)
      winner       <- Future.fromTry(winnerOption.fold[Try[User]](Failure(TopAuthorNotFound))(user => Success(user)))
    } yield winner
}

object FutureCompetitionStart extends App {
  import _root_.service.TwitterServiceFuture
  import twitter.{LocalTwitterApi, TwitterApi}

  import scala.concurrent.Await
  import scala.concurrent.duration.DurationInt
  import scala.util.Random

  implicit val ec: ExecutionContext = ExecutionContext.global

  val api: TwitterApi = new LocalTwitterApi(Iterator.continually((Random.nextDouble() * 1000).toInt))

  val service: TwitterService[Future] = new TwitterServiceFuture(api)

  val methods: CompetitionMethods[Future] = new CompetitionMethods[Future](service)

  val oleg: User   = User("oleg")
  val ivan: User   = User("ivan")
  val marya: User  = User("marya")
  val gustav: User = User("gustav")
  val bot: User    = User("bot")

  val users: List[User] = List(oleg, ivan, marya, gustav, bot)

  val followers: Map[User, List[User]] = Map(
    oleg   -> List(ivan, bot),
    ivan   -> List(oleg, gustav),
    marya  -> List(oleg, ivan, gustav, bot),
    gustav -> List(oleg, ivan, marya),
    bot    -> List(bot)
  )

  private val winner: User =
    Await.result(new FutureCompetition(service, methods).winner(users, followers, bot), 30.seconds)
  println(s"${winner.id} win!!!")
}
