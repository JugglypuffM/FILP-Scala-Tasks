package registry.adapter.jdbc.sqlite

import cats.effect._
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.compatible.Assertion
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import registry.domain.model.{Application, Passport, PhoneNumber, User}
import registry.domain.service.ApplicationAlg

import scala.concurrent.duration._

class ApplicationAlgTest extends AsyncFreeSpec with AsyncIOSpec with Matchers {

  def stand: (ApplicationAlg[IO] => IO[Assertion]) => IO[Assertion] = SQLiteStand(ApplicationAlg.build[IO])

  val testAppId: Application.Id = Application.Id("appId")
  val testUser: User = User(
    "Иван",
    "Иванов",
    Some("Иванович"),
    Passport.parseUnsafe("1234567890"),
    PhoneNumber.parseUnsafe("+79993332222")
  )

  "ApplicationAlg implementation via sqlite " - {
    "should " - {
      "persist application to database" in stand { appAlg =>
        for {
          _     <- appAlg.persist(testAppId, testUser, 1.seconds)
          appId <- appAlg.getApplicationBy(testUser)
          user  <- appAlg.getUserBy(testAppId)
        } yield (appId, user) shouldBe (Some(testAppId), Some(testUser))
      }

      "remove application from database" in stand { appAlg =>
        for {
          _     <- appAlg.persist(testAppId, testUser, 1.minute)
          _     <- appAlg.remove(testAppId)
          appId <- appAlg.getApplicationBy(testUser)
          user  <- appAlg.getUserBy(testAppId)
        } yield (appId, user) shouldBe (None, None)
      }

      "exclude outdated application" in stand { appAlg =>
        for {
          _     <- appAlg.persist(testAppId, testUser, 100.millis)
          _     <- IO.sleep(110.millis)
          appId <- appAlg.getApplicationBy(testUser)
          user  <- appAlg.getUserBy(testAppId)
        } yield (appId, user) shouldBe (None, None)
      }
    }
  }

}
