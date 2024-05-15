package registry.adapter.jdbc.sqlite

import cats.effect._
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.compatible.Assertion
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import registry.domain.model._
import registry.domain.service.UserAlg

class UserAlgTest extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  def stand: (UserAlg[IO] => IO[Assertion]) => IO[Assertion] = SQLiteStand(UserAlg.build[IO])

  val testUser: User = User("Иван", "Иванов", Some("Иванович"), Passport.parseUnsafe("1234567890"), PhoneNumber.parseUnsafe("+79993332222"))

  "UserAlg implementation via sqlite " - {
    "should " - {
      "persist user to database" in stand { userAlg =>
        for {
          _    <- userAlg.persist(testUser)
          user <- userAlg.getBy(testUser.passport)
        } yield user shouldBe Some(testUser)
      }
    }
  }
}

