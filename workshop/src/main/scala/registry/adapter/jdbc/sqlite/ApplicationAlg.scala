package registry.adapter.jdbc.sqlite

import cats.effect._
import cats.syntax.all._
import doobie._
import doobie.implicits._
import registry.domain.model.{Application, Passport, PhoneNumber, User}
import registry.domain.service.ApplicationAlg

import scala.concurrent.duration.FiniteDuration

object ApplicationAlg {

  def build[F[_]: MonadCancelThrow](transactor: Transactor[F]): F[ApplicationAlg[F]] =
    for {
      _ <- createTable.transact(transactor)
      appAlg = new ApplicationAlgImpl[F](transactor)
    } yield appAlg

  //TODO: сделать так, чтобы учитывалось время жизни заявки на регистрацию
  private class ApplicationAlgImpl[F[_]: MonadCancelThrow](transactor: Transactor[F]) extends ApplicationAlg[F] {

    override def getApplicationBy(user: User): F[Option[Application.Id]] =
      for {
        response <- sql"""
                    SELECT id
                    FROM application
                    WHERE passport = ${user.passport.serial.value ++ user.passport.number.value}
                  """
          .query[String]
          .to[List]
          .transact(transactor)
          .map(_.headOption)
        res <- response match {
          case Some(id) =>
            val appId = Application.Id(id)
            appId.some.pure[F]
          case None => None.pure[F]
        }
      } yield res

    override def getUserBy(appId: Application.Id): F[Option[User]] =
      for {
        response <- sql"""
                    SELECT passport, name, surname, patronymic, phone
                    FROM application
                    WHERE id = ${appId.value}
                  """
          .query[(String, String, String, Option[String], String)]
          .to[List]
          .transact(transactor)
          .map(_.headOption)
        res <- response match {
          case Some((passport, name, surname, patronymic, phone)) =>
            User(name, surname, patronymic, Passport.parseUnsafe(passport), PhoneNumber.parseUnsafe(phone)).some.pure[F]
          case None => None.pure[F]
        }

      } yield res

    override def persist(appId: Application.Id, user: User, ttl: FiniteDuration): F[Unit] = sql"""
      INSERT INTO application (id, passport, name, surname, patronymic, phone) VALUES (
        ${appId.value},
        ${user.passport.serial.value + user.passport.number.value},
        ${user.name},
        ${user.surname},
        ${user.patronymic},
        ${user.phoneNumber.country + user.phoneNumber.code + user.phoneNumber.number},
      )
    """.update.run.transact(transactor).as(())

    override def remove(app: Application.Id): F[Unit] = sql"""
      DELETE FROM application WHERE id = ${app.value}
    """.update.run.transact(transactor).as(())

  }

  private val createTable = sql"""
     CREATE TABLE IF NOT EXISTS application (
       id TEXT NOT NULL UNIQUE,
       passport TEXT NOT NULL UNIQUE,
       name TEXT NOT NULL,
       surname TEXT NOT NULL,
       patronymic TEXT,
       phone TEXT NOT NULL
     )
  """.update.run
}
