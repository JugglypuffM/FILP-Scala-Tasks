package registry.domain.service

import cats.effect._
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all._
import org.scalamock.scalatest.AsyncMockFactory
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import registry.domain.model._

class EventHandlerAlgImplTest extends AsyncFreeSpec with AsyncIOSpec with Matchers with AsyncMockFactory {
  trait Stand {
    val userAlg: UserAlg[IO]       = mock[UserAlg[IO]]
    val appAlg: ApplicationAlg[IO] = mock[ApplicationAlg[IO]]

    val eventHandler: EventHandlerAlg[IO] = EventHandlerAlg.build[IO](userAlg, appAlg)
  }

  private def stand = new Stand {}

  "EventHandlerAlgImpl " - {
    "should" - {
      "throw exception when user is not found by application id" in {
        val s = stand
        import s._
        val appId = Application.Id("appId")

        (appAlg.getUserBy _).expects(appId).returning(None.pure[IO]).once

        val result = eventHandler.handle(ApprovalEvent(appId, ApprovalEvent.Result.Trust))
        result.attempt.asserting(_ shouldBe Left(EventHandlerAlg.Error.UserNotFound))
      }

      "handle approval application" in {
        val s = stand
        import s._
        val appId = Application.Id("appId")
        val user = User(
          "Иван",
          "Иванов",
          Some("Иванович"),
          Passport.parseUnsafe("1234567890"),
          PhoneNumber.parseUnsafe("+79993332222")
        )
        (appAlg.getUserBy _).expects(appId).returning(user.some.pure[IO]).once
        (userAlg.persist _).expects(user).returning(().pure[IO]).once
        (appAlg.remove _).expects(appId).returning(().pure[IO]).once
        val result = eventHandler.handle(ApprovalEvent(appId, ApprovalEvent.Result.Trust))
        result.asserting(_ shouldBe ())
      }

      "remove application when event is mistrust" in {
        val s = stand
        import s._

        (appAlg.remove _).expects(Application.Id("appId")).returning(().pure[IO]).once
        val result = eventHandler.handle(ApprovalEvent(Application.Id("appId"), ApprovalEvent.Result.Mistrust))

        result.asserting(_ shouldBe ())
      }
    }
  }
}
