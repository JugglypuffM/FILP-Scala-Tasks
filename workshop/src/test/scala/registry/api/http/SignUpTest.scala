package registry.api.http
import cats.effect._
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all._
import io.circe.syntax._
import org.http4s._
import org.http4s.headers.Accept
import org.http4s.implicits._
import org.scalamock.scalatest.AsyncMockFactory
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import registry.domain.Registry

class SignUpTest extends AsyncFreeSpec with AsyncIOSpec with Matchers with AsyncMockFactory {

  trait Stand {
    val registry: Registry[IO] = mock[Registry[IO]]
    val checkIn: SignUp[IO] = new SignUp[IO](registry)
  }

  def stand: Stand = new Stand {}

  val request: Request[IO] = Request[IO](
    method = Method.POST,
    uri = uri"/signUp",
    headers = Headers(
      Accept(MediaType.application.json)
    )
  )
  val entity: Map[String, String] = Map(
    "name" -> "Иван",
    "surname" -> "Иванов",
    "patronymic" -> "Иванович",
    "passport" -> "1234567890",
    "phoneNumber" -> "+79993332222",
  )

  "CheckIn" - {
    "should" - {
      "return success response" in {
        val s = stand
        import s._

        (registry.signUp _).expects(*).returning(().pure[IO])

        for {
          r <- checkIn.route.orNotFound.run(request.withEntity(entity.asJson.toString))
          body <- r.body.compile.toList.map(_.toArray).map(x => new String(x, "UTF-8"))
        } yield {
          r.status shouldBe Status.Ok
          body shouldBe "Ваша заявка на регистрацию принята"
        }
      }

      "return BadRequest when body request is incorrect" in {
        val s = stand
        import s._

        for {
          r <- checkIn.route.orNotFound.run(request.withEntity(Map("kek" -> "cheburek").asJson.toString))
          body <- r.body.compile.toList.map(_.toArray).map(x => new String(x, "UTF-8"))
        } yield {
          r.status shouldBe Status.BadRequest
          body shouldBe
            """Invalid message body: Could not decode JSON: {
              |  "kek" : "cheburek"
              |}""".stripMargin
        }
      }
      "return BadRequest when param of request is invalid" in {
        val s = stand
        import s._

        for {
          r <- checkIn.route.orNotFound.run(request.withEntity(entity.map(x => x._1 -> (x._2 + "1")).asJson.toString()))
          body <- r.body.compile.toList.map(_.toArray).map(x => new String(x, "UTF-8"))
        } yield {
          r.status shouldBe Status.BadRequest
          body shouldBe """Форма заполнена неверно:
                          |- Имя содержит некорректные символы
                          |- Фамилия содержит некорректные символы
                          |- Отчество содержит некорректные символы
                          |- Данные паспорта заполнены неверно
                          |- Номер телефона указан неверно""".stripMargin
        }
      }
      "return BadRequest when user already has an application" in {
        val s = stand
        import s._

        (registry.signUp _).expects(*).returning(IO.raiseError(Registry.Error.UserApplicationAlreadyExists))

        for {
          r <- checkIn.route.orNotFound.run(request.withEntity(entity.asJson.toString))
          body <- r.body.compile.toList.map(_.toArray).map(x => new String(x, "UTF-8"))
        } yield {
          r.status shouldBe Status.BadRequest
          body shouldBe "Заявка на регистрацию уже заведена"
        }
      }

      "return BadRequest when user is already registered" in {
        val s = stand
        import s._

        (registry.signUp _).expects(*).returning(IO.raiseError(Registry.Error.UserAlreadyExists))

        for {
          r <- checkIn.route.orNotFound.run(request.withEntity(entity.asJson.toString))
          body <- r.body.compile.toList.map(_.toArray).map(x => new String(x, "UTF-8"))
        } yield {
          r.status shouldBe Status.BadRequest
          body shouldBe "Данный пользователь уже зарегистрирован в системе"
        }
      }

      "return InternalServerError when an unknown error occurred" in {
        val s = stand
        import s._

        (registry.signUp _).expects(*).returning(IO.raiseError(new Exception("FAIL!!1!")))

        for {
          r <- checkIn.route.orNotFound.run(request.withEntity(entity.asJson.toString))
          body <- r.body.compile.toList.map(_.toArray).map(x => new String(x, "UTF-8"))
        } yield {
          r.status shouldBe Status.InternalServerError
          body shouldBe "Произошла ошибка, повторите попытку позже: FAIL!!1!"
        }
      }
    }
  }
}
