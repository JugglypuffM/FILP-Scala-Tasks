package registry.domain

import cats.effect._
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all._
import org.scalamock.scalatest.AsyncMockFactory
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import registry.domain.model._
import registry.domain.service._

class RegistryImplTest extends AsyncFreeSpec with AsyncIOSpec with Matchers with AsyncMockFactory {

  trait Stand {
    val userAlg: UserAlg[IO]                      = mock[UserAlg[IO]]
    val appAlg: ApplicationAlg[IO]                = mock[ApplicationAlg[IO]]
    val trustwortinessAlg: TrustworthinessAlg[IO] = mock[TrustworthinessAlg[IO]]
    val registry: Registry[IO]                    = Registry.build[IO](userAlg, appAlg, trustwortinessAlg)
  }

  def stand: Stand = new Stand {}

  val testAppId: Application.Id = Application.Id("appId")
  val testUser: User = User(
    "Иван",
    "Иванов",
    Some("Иванович"),
    Passport.parseUnsafe("1234567890"),
    PhoneNumber.parseUnsafe("+79993332222")
  )

  "RegistryImpl" - {
    "should" - {
      "throw exception when user is already registered" in {
        val s = stand
        import s._

        (userAlg.getBy _).expects(testUser.passport).returning(testUser.some.pure[IO]).once

        registry
          .signUp(testUser)
          .attempt
          .asserting(_ shouldBe Left(Registry.Error.UserAlreadyExists))
      }

      "throw exception when application is already exists" in {
        val s = stand
        import s._
        (userAlg.getBy _).expects(testUser.passport).returning(none.pure[IO]).once
        (appAlg.getApplicationBy _).expects(testUser).returning(testAppId.some.pure[IO]).once

        registry
          .signUp(testUser)
          .attempt
          .asserting(_ shouldBe Left(Registry.Error.UserApplicationAlreadyExists))
      }

      "send request to trustworhiness service and save application to storage" in {
        val s = stand
        import s._
        (userAlg.getBy _).expects(testUser.passport).returning(none.pure[IO]).once
        (appAlg.getApplicationBy _).expects(testUser).returning(none.pure[IO]).once
        (trustwortinessAlg.check _).expects(testUser).returning(testAppId.pure[IO]).once
        (appAlg.persist _).expects(testAppId, testUser, *).returning(().pure[IO]).once

        registry
          .signUp(testUser)
          .asserting(_ shouldBe ())
      }
    }
  }
}
