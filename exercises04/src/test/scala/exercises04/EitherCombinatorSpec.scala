package exercises04

import org.scalatest.wordspec.AnyWordSpec
import either.EitherCombinators._

case class Error(message: String)

class EitherCombinatorSpec extends AnyWordSpec {
  "Either.map" should {
    "work" in {
      assert(Left[Error, Int](Error("abc")).map(_ => 10) == Left[Error, Int](Error("abc")))
      assert(Right("10").map(_.toInt) == Right(10))
    }
  }

  "Either.flatMap" should {
    "work" in {
      assert(Left[Error, Int](Error("abc")).flatMap(_ => Left(Error("42"))) == Left(Error("abc")))
      assert(Left[Error, Int](Error("abc")).flatMap(_ => Right(42)) == Left(Error("abc")))
      assert(Right("42").flatMap(v => Left(Error(v))) == Left(Error("42")))
      assert(Right("42").flatMap(str => Right(str.toInt)) == Right(42))
    }
  }

  "Either.orElse" should {
    "work" in {
      assert((Left(Error("abc")) orElse Left(Error("42"))) == Left(Error("abc")))
      assert((Left(Error("abc")) orElse Right(42)) == Right(42))
      assert((Right(42) orElse Left(Error("abc"))) == Right(42))
      assert((Right(42) orElse Right("abc")) == Right(42))
    }
  }

  "Either.map2" should {
    "work" in {
      assert(Left[Error, Int](Error("abc")).map2(Left(Error("42")))(_ + _) == Left(Error("abc")))
      assert(Left[Error, Int](Error("abc")).map2(Right(42))(_ + _) == Left(Error("abc")))
      assert(Right(42).map2(Left(Error("abc")))(_ + _) == Left(Error("abc")))
      assert(Right(42).map2(Right(100500))(_ + _) == Right(100542))
    }
  }

  "Either.fromOption" should {
    "work" in {
      assert(Either.fromOption(None)(Error("abc")) == Left(Error("abc")))
      assert(Either.fromOption(Some(42))(Error("abc")) == Right(42))
    }
  }

  "Either.traverse" should {
    "work" in {
      val f: String => Either[Error, Int] = str =>
        str.toIntOption match {
          case Some(num) => Right(num)
          case None      => Left(Error("it's not a number"))
        }

      assert(Either.traverse(Nil)(f) == Right(Nil))
      assert(Either.traverse(List("1", "2", "3", "4", "5"))(f) == Right(List(1, 2, 3, 4, 5)))
      assert(Either.traverse(List("1", "2", "3", "abc", "5"))(f) == Left(Error("it's not a number")))
      assert(Either.traverse(List("1", "2", "3", "abc", "def", "ghi"))(f) == Left(Error("it's not a number")))
    }
  }

  "Either.sequence" should {
    "work" in {
      assert(Either.sequence(Nil) == Right(Nil))
      assert(Either.sequence(List(Right(1), Right(2), Right(3), Right(4), Right(5))) == Right(List(1, 2, 3, 4, 5)))
      assert(Either.sequence(List(Right(1), Right(2), Right(3), Left("abc"), Right(5))) == Left("abc"))
      assert(Either.sequence(List(Right(1), Right(2), Right(3), Left("abc"), Left("def"), Left("ghi"))) == Left("abc"))
    }
  }
}
