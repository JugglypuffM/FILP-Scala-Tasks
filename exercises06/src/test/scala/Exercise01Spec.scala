import exercises06.data.NonEmptyList
import exercises06.typeclasses._
import org.scalatest.wordspec.AnyWordSpec
import exercises06.ex01.Exercise01.Instances._
import exercises06.ex01.Exercise01.Syntax._

class Exercise01Spec extends AnyWordSpec {
  private case class OwnList[A](list: List[A])

  private implicit def ownListInstances(
      implicit underlying: Traverse[List] with Applicative[List] with Functor[List]
  ): Traverse[OwnList] with Applicative[OwnList] =
    new Traverse[OwnList] with Applicative[OwnList] {
      def traverse[G[_]: Applicative, A, B](fa: OwnList[A])(f: A => G[B]): G[OwnList[B]] =
        Functor[G].map(underlying.traverse(fa.list)(f))(OwnList(_))

      def ap[A, B](ff: OwnList[A => B])(fa: OwnList[A]): OwnList[B] =
        OwnList(underlying.ap(ff.list)(fa.list))

      def pure[A](x: A): OwnList[A] =
        OwnList(underlying.pure(x))

      def map[A, B](fa: OwnList[A])(f: A => B): OwnList[B] =
        OwnList(underlying.map(fa.list)(f))

      def foldLeft[A, B](fa: OwnList[A], b: B)(f: (B, A) => B): B =
        underlying.foldLeft(fa.list, b)(f)
    }

  private implicit def ownListMonoid[A](implicit underlying: Monoid[List[A]]): Monoid[OwnList[A]] =
    new Monoid[OwnList[A]] {
      override def empty: OwnList[A] = OwnList(underlying.empty)

      override def combine(x: OwnList[A], y: OwnList[A]): OwnList[A] = OwnList(underlying.combine(x.list, y.list))
    }

  "Exercise01" should {
    "monoid" in {
      assert((1 |+| 2) == 3)
      assert(("a" |+| "b") == "ab")
      assertDoesNotCompile("1 |+| \"2\"")
      assertDoesNotCompile("\"2\" |+| 1")
      assert((NonEmptyList.of(1, 3, 2) |+| NonEmptyList.of(5, 4)) == NonEmptyList(1, List(3, 2, 5, 4)))
      assert((List(2, 1, 3) |+| List(4, 5)) == List(2, 1, 3, 4, 5))
      assert((List.empty[String] |+| List.empty[String]) == List.empty[String])
      assertDoesNotCompile("(List.empty[String] |+| List.empty[Int])")
      assertDoesNotCompile("(List(1,2,3) |+| List.empty[String])")
      assert((List.empty[String] |+| List("a", "b", "c")) == List("a", "b", "c"))

      assert((OwnList(List(1, 2)) |+| OwnList(List(4, 5, 6))) == OwnList(List(1, 2, 4, 5, 6)))
    }

    "pure" in {
      assert(1.pure[Option].contains(1))
      assert(1.pure[List] == List(1))
      assert("a".pure[List] == List("a"))
      assert(2.pure[NonEmptyList] == NonEmptyList(2, List()))

      assert(2.pure[OwnList] == OwnList(List(2)))
    }

    "foldable" in {
      assert(List("a", "b", "c").combineAll == "abc")
      assert(List(1, 2, 3).combineAll == 6)
      assert(List(List(1, 2, 3), List(3, 4, 5)).combineAll == List(1, 2, 3, 3, 4, 5))
      assert(NonEmptyList.of("a", "b", "c").combineAll == "abc")
      assert(NonEmptyList.of(1, 2, 3).combineAll == 6)
      assert(NonEmptyList.of(2, 3, 4).foldLeft(1)(_ * _) == 24)
      assert(List.empty[Int].combineAll == 0)

      assert(OwnList(List("a", "b", "c")).combineAll == "abc")
      assert(OwnList(List(1, 2, 3)).combineAll == 6)
      assert(
        OwnList(List(OwnList(List(1, 2, 3)), OwnList(List(3, 4, 5)))).combineAll == OwnList(List(1, 2, 3, 3, 4, 5))
      )
      assert(OwnList(List.empty[Int]).combineAll == 0)
      assert(OwnList(List(2, 3, 4)).foldLeft(1)(_ * _) == 24)
    }

    "applicative" in {
      assert(List(1, 2, 3).aproduct(List("a", "b", "c")) == List((1, "a"), (2, "b"), (3, "c")))
      assert(List(1, 2, 3).aproduct(List("a", "b")) == List((1, "a"), (2, "b")))
      assert(List(1, 2).aproduct(List("a", "b", "c", "d")) == List((1, "a"), (2, "b")))

      assert(Option(1).aproduct(Option("a")).contains((1, "a")))
      assert(Option(1).aproduct(None).isEmpty)
      assert(None.aproduct(Option(1)).isEmpty)
      assert(None.aproduct(None).isEmpty)
      assert(NonEmptyList.of(1, 2, 3).aproduct(NonEmptyList.of(4, 5, 6)) == NonEmptyList((1, 4), List((2, 5), (3, 6))))
      assert(NonEmptyList.of(1, 2).aproduct(NonEmptyList.of(4, 5, 6)) == NonEmptyList((1, 4), List((2, 5))))

      assert(
        OwnList(List(1, 2, 3)).aproduct(OwnList(List("a", "b", "c"))) == OwnList(List((1, "a"), (2, "b"), (3, "c")))
      )
    }

    "traverse" in {
      assert(List(1, 2, 3, 8).traverse(getTag).contains(List("Less", "Less", "Less", "Greater")))
      assert(List(6, 10).traverse(getTag).isEmpty)
      assert(List.empty[Int].traverse(getTag).contains(List()))
      assert(Option(1).traverse(getTag).contains(Some("Less")))
      assert(Option(6).traverse(getTag).isEmpty)
      assert(Option.empty[Int].traverse(getTag).contains(None))
      assert(
        NonEmptyList.of(1, 2, 3, 8).traverse(getTag).contains(NonEmptyList("Less", List("Less", "Less", "Greater")))
      )
      assert(NonEmptyList.of(6, 10).traverse(getTag).isEmpty)

      assert(OwnList(List(1, 2, 3, 8)).traverse(getTag).contains(OwnList(List("Less", "Less", "Less", "Greater"))))

      def getTag(id: Int): Option[String] =
        if (id < 5) Some("Less")
        else if (id > 7) Some("Greater")
        else None
    }

    "functor" in {
      assert(NonEmptyList.of(1, 2, 3).map(_ + 1) == NonEmptyList(2, List(3, 4)))
      assert(OwnList(List(1, 2, 3)).map(_ + 1) == OwnList(List(2, 3, 4)))
      assert(OwnList(List(1, 2, 3)).map(identity) == OwnList(List(1, 2, 3)))
      assert(OwnList(List(1, 2, 3)).map(_ + 1).map(_ + 2) == OwnList(List(1, 2, 3)).map(((_: Int) + 1) andThen (_ + 2)))
      assert(OwnList(List[Int]()).map(_ + 1) == OwnList(List()))
    }
  }
}
