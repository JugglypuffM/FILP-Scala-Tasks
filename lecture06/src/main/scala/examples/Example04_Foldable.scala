package examples

import examples.typeclasses.{Foldable, Monoid}
import examples.typeclasses.FoldableSyntax._
import examples.typeclasses.FoldableInstances._
import examples.typeclasses.MonoidInstances._
import examples.typeclasses.MonoidSyntax.MonoidOps

object Example04_Foldable extends App {

  val vectorSum: Int = Vector(1, 2, 3).foldLeft(0) { case (a, b) => a + b } // result: 6
  println(vectorSum)

  val listSum: Int = List(1, 2, 3).foldLeft(0) { case (a, b) => a + b } // result: 6
  println(listSum)

  val optionSum: Int = Option(1).foldLeft(0) { case (a, b) => a + b } // result: 1
  println(optionSum)

  // Foldable

  // List implementation
  def combineAll[A: Monoid](as: List[A]): A = {
    as.foldLeft(Monoid[A].empty)(_ |+| _)
  }

  // Common implementation
  def combineAllCommon[F[_]: Foldable, A: Monoid](as: F[A]): A = {
    as.foldLeft[A](Monoid[A].empty)(_ |+| _)
  }

  val listResult: Int = combineAllCommon(List(1, 2, 3, 4, 5))
  println(listResult)

  val vectorResult: Int = combineAllCommon(Vector(1, 2, 3, 4, 5))
  println(vectorResult)

  val optResult: Int = combineAllCommon(Option(42))
  println(optResult)
}
