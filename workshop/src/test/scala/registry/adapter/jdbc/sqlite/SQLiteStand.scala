package registry.adapter.jdbc.sqlite
import cats.effect._
import com.zaxxer.hikari.HikariConfig
import doobie._
import doobie.hikari._
import org.scalatest.compatible.Assertion

object SQLiteStand {
  def apply[A](init: Transactor[IO] => IO[A]): (A => IO[Assertion]) => IO[Assertion] = (app: A => IO[Assertion]) => {
    val transactor: Resource[IO, HikariTransactor[IO]] = {
      val config = new HikariConfig()
      config.setDriverClassName("org.sqlite.JDBC")
      config.setJdbcUrl("jdbc:sqlite::memory:")
      config.setMaximumPoolSize(1)
      HikariTransactor.fromHikariConfig[IO](config, None)
    }
    transactor.use { t =>
      for {
        alg    <- init(t)
        result <- app(alg)
      } yield result
    }
  }

}
