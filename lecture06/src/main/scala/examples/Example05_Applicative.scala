package examples

import examples.Example06_Applicative_Eval.Temperature
import examples.typeclasses.{Applicative, Functor}
import examples.typeclasses.ApplicativeInstances._
import examples.typeclasses.ApplicativeSyntax._
import examples.typeclasses.FunctorSyntax.FunctorOps

import scala.util.Try

object Example05_Applicative extends App {

  // --------------
  // Pure
  Applicative[Option].pure(42)
  Applicative[List].pure(42)

  val pureOption: Option[Int] = 42.pure[Option] // Some(42)
  val pureList: List[Int]     = 42.pure[List]   // List(42)

  // --------------
  // Product

  def getTemperature[F[_]: Applicative](city: String): F[Temperature] = {
    city match {
      case "Moscow"        => Applicative[F].pure(Temperature(-5))
      case "Yekaterinburg" => Applicative[F].pure(Temperature(-10))
      case "Sochi"         => Applicative[F].pure(Temperature(15))
    }
  }

  val res2: Option[(Temperature, Temperature)] = Applicative[Option].product(
    getTemperature[Option]("Moscow"),
    getTemperature[Option]("Sochi")
  )
  println(res2)

  val res3: Try[(Temperature, Temperature)] = Applicative[Try].product(
    getTemperature[Try]("Moscow"),
    getTemperature[Try]("Sochi")
  )
  println(res3)

  // ----------
  // mapN
  val res4: Option[(Temperature, Temperature, Temperature)] = (
    getTemperature[Option]("Moscow"),
    getTemperature[Option]("Yekaterinburg"),
    getTemperature[Option]("Sochi")
  ).mapN { case (a, b, c) => (a, b, c) }

  println(res4)

  // Applicative Laws
  val fa: Option[String] = Some("a")
  val fb: Option[String] = Some("b")
  val fc: Option[String] = Some("c")

  def associativity[F[_]: Applicative, A, B, C](
      fa: F[A],
      fb: F[B],
      fc: F[C]
  ): Boolean = {
    fa.product(fb.product(fc)) == fa.product(fb).product(fc).map { case ((a, b), c) => (a, (b, c)) }
    // (a, (b, c)) ~ ((a, b), c)
  }

  val associativityResult: Boolean = associativity(fa, fb, fc)
  println(associativityResult)

  // Right identity
  def rightIdentity[F[_]: Applicative, A](fa: F[A]): Boolean = {
    fa.product(().pure[F]).map(_._1) == fa
    // (a, ()) ~ (a)
  }

  val rightIdentityResult: Boolean = rightIdentity(fa)
  println(rightIdentityResult)

  // Left identity
  def leftIdentity[F[_]: Applicative, A](fa: F[A]): Boolean = {
    ().pure[F].product(fa).map(_._2) == fa
    // ((), a ~ (a)
  }

  val leftIdentityResult: Boolean = leftIdentity(fa)
  println(leftIdentityResult)
}
