package examples.typeclasses

import examples.typeclasses.ApplicativeSyntax.{FOps, Ops}
import examples.typeclasses.FunctorSyntax.FunctorOps

/*
`traverse` принимает структуру данных `F[A]` и функцию `f: A => G[B]`,
 где `G[_]` — это некоторый аппликативный функтор.
 Задача `traverse` — применить `f` к каждому элементу `A` в `F`,
 а затем собрать результаты обратно в `F[B]`
 */

trait Traverse[F[_]] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
}

object Traverse {
  def apply[F[_]: Traverse]: Traverse[F] = implicitly
}

object TraverseInstances {

  // Инстанс Traversable для List
  implicit val listTraverse: Traverse[List] = new Traverse[List] {
    def traverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] =
      fa.foldLeft(List.empty[B].pure[G])((accF, next) =>
        accF.product(f(next)).map {
          case (acc, next) => acc.appended(next)
        }
      )
  }

  // Инстанс для Option
  implicit val optionTraverse: Traverse[Option] = new Traverse[Option] {
    def traverse[G[_]: Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] =
      fa match {
        case Some(value) => f(value).map(Some(_))
        case None        => Option.empty[B].pure[G]
      }
  }
}

object TraverseSyntax {
  implicit class TraverseOps[F[_], G[_], A](private val fa: F[A]) extends AnyVal {
    def traverse[B](f: A => G[B])(implicit a: Applicative[G], t: Traverse[F]): G[F[B]] =
      t.traverse(fa)(f)
  }

  implicit class SequenceOps[F[_], G[_], A](private val gfa: G[F[A]]) extends AnyVal {
    def sequence(implicit applicative: Applicative[F], traverse: Traverse[G]): F[G[A]] =
      traverse.traverse(gfa)(identity)
  }
}
