package examples

import examples.typeclasses.Monad
import examples.typeclasses.MonadInstances._
import examples.typeclasses.MonadSyntax._

object Example02_MonadLaws extends App {

  val a: String          = "123"
  val fa: Option[String] = Some("123")

  val f: String => Option[Int] = _.toIntOption
  val g: Int => Option[Double] = _.toDouble.pure[Option]

  // Left identity
  val leftIdentity: Boolean = Monad[Option].pure(a).flatMap(f) == f(a)
  println(leftIdentity)

  // Right identity
  val rightIdentity: Boolean = fa.flatMap(Monad[Option].pure) == fa
  println(rightIdentity)

  // Associativity
  val associativity: Boolean = fa.flatMap(f).flatMap(g) == fa.flatMap(a => f(a).flatMap(g))
  println(associativity)
}
