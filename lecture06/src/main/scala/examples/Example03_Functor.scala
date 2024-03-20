package examples

import examples.typeclasses.Functor
import examples.typeclasses.FunctorInstances._
import examples.typeclasses.FunctorSyntax._

object Example03_Functor extends App {

  Functor[List].map(List(1, 2, 3))(_ + 1)
  Functor[Option].map(Some(1))(_ + 1)

  // ---

  def checkMeaningOfLife(num: Int): String = {
    if (num == 42) s"$num is meaning of life"
    else s"$num isn't meaning of life"
  }

  def functorUsingExample[F[_]: Functor](numF: F[Int]): F[String] = {
    // Последовательность вычислений с map
    numF
      .map(checkMeaningOfLife)
      .map(_.toUpperCase)
      .map(_ + "!")
  }

  val res1: Option[String] = functorUsingExample[Option](Some(42)) // result: Some(42 IS MEANING OF LIFE)
  println(res1)
  val res2: Option[String] = functorUsingExample[Option](None) // result: None
  println(res2)

  val res3: List[String] = functorUsingExample(List(1, 2, 42))
  println(res3)
  // result: List(
  //   1 ISN'T MEANING OF LIFE!,
  //   2 ISN'T MEANING OF LIFE!,
  //   42 IS MEANING OF LIFE!
  // )

  val res4: Vector[String] = functorUsingExample(Vector(1, 2, 42))
  println(res4)
  // result: Vector(
  //   1 ISN'T MEANING OF LIFE!,
  //   2 ISN'T MEANING OF LIFE!,
  //   42 IS MEANING OF LIFE!
  // )

  // Laws

  // Identity
  def identity[A](a: A): A = a

  val fa: List[Int]          = List(1, 2, 3)
  val identity_res1: Boolean = Functor[List].map(fa)(x => x) == fa
  println(identity_res1) // True
  // или
  val identity_res2: Boolean = Functor[List].map(fa)(identity) == fa
  println(identity_res2) // True

  // Composition
  val f: Int => Long    = _.toLong
  val g: Long => String = _.toString

  val composition_res: Boolean = fa.map(a => g(f(a))) == fa.map(f).map(g)
  println(composition_res) // True
}
