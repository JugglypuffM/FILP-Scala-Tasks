package exercises04

import exercises04.either.EitherCombinators._
import exercises04.parser.Error._
import exercises04.parser.Examples._
import exercises04.parser._
import org.scalatest.wordspec.AnyWordSpec

class ExamplesSpec extends AnyWordSpec {
  "Examples.transformToOption" should {
    "если rawUser.firstName или rawUser.secondName == None, то функция должна вернуть None" in {
      assert(
        transformToOption(RawUser(Long.MaxValue.toString, banned = "false", "incorrect", None, Some("Odersky"), None)).isEmpty
      )
      assert(
        transformToOption(RawUser(Long.MaxValue.toString, banned = "false", "incorrect", Some("Martin"), None, None)).isEmpty
      )
      assert(
        transformToOption(RawUser(Long.MaxValue.toString, banned = "false", "incorrect", None, None, None)).isEmpty
      )
    }

    "passport должен быть передан в формате 1234 567890, если не так, то функция должна вернуть None" in {
      assert(
        transformToOption(
          RawUser(Long.MaxValue.toString, banned = "false", "incorrect", Some("Martin"), Some("Odersky"), None)
        ).isEmpty
      )
    }

    "если rawUser.id не парсится в Long то функция должна вернуть None" in {
      assert(
        transformToOption(
          RawUser("Long.MaxValue.toString", banned = "false", "incorrect", Some("Martin"), Some("Odersky"), None)
        ).isEmpty
      )
    }

    "если rawUser.banned, то вернуть None" in {
      assert(
        transformToOption(
          RawUser(Long.MaxValue.toString, banned = "true", "incorrect", Some("Martin"), Some("Odersky"), None)
        ).isEmpty
      )
    }

    "корректно трансформировать" in {
      assert(
        transformToOption(
          RawUser(Long.MaxValue.toString, banned = "false", "1234 567890", Some("Martin"), Some("Odersky"), None)
        ).contains(User(Long.MaxValue, UserName("Martin", "Odersky", None), Passport(1234, 567890)))
      )

      assert(
        transformToOption(
          RawUser(
            Long.MaxValue.toString,
            banned = "false",
            "1234 567890",
            Some("Martin"),
            Some("Odersky"),
            Some("")
          )
        ).contains(User(Long.MaxValue, UserName("Martin", "Odersky", Some("")), Passport(1234, 567890)))
      )
    }

    "invalid banned" should {
      "return None" in {
        assert(
          transformToOption(
            RawUser(Long.MaxValue.toString, banned = "invalid", "1234sdf567890", None, None, None)
          ) == None
        )
      }
    }
  }

  "Examples.transformToEither" should {
    "return right" in {
      assert(
        transformToEither(
          RawUser(Long.MaxValue.toString, banned = "false", "1234 567890", Some("Martin"), Some("Odersky"), None)
        )
          == Right(User(Long.MaxValue, UserName("Martin", "Odersky", None), Passport(1234, 567890)))
      )

      assert(
        transformToEither(
          RawUser(
            Long.MaxValue.toString,
            banned = "false",
            "1234 567890",
            Some("Martin"),
            Some("Odersky"),
            Some("")
          )
        )
          == Right(User(Long.MaxValue, UserName("Martin", "Odersky", Some("")), Passport(1234, 567890)))
      )
    }

    "return errors in Left with correct priority" in {
      assert(transformToEither(RawUser("abc", banned = "true", "incorrect", None, None, None)) == Left(Banned))
      assert(
        transformToEither(RawUser("Long.MaxValue.toString", banned = "false", "incorrect", None, None, None)) == Left(
          InvalidId
        )
      )
      assert(
        transformToEither(RawUser(Long.MaxValue.toString, banned = "false", "incorrect", None, None, None)) == Left(
          InvalidName
        )
      )
      assert(
        transformToEither(
          RawUser(Long.MaxValue.toString, banned = "false", "incorrect", Some("Martin"), None, None)
        ) == Left(InvalidName)
      )
      assert(
        transformToEither(
          RawUser(Long.MaxValue.toString, banned = "false", "incorrect", None, Some("Odersky"), None)
        ) == Left(InvalidName)
      )
      assert(
        transformToEither(
          RawUser(Long.MaxValue.toString, banned = "false", "incorrect", Some("Martin"), Some("Odersky"), None)
        ) == Left(InvalidPassport)
      )
    }
    "invalid banned" should {
      "return InvalidBanned" in {
        assert(
          transformToEither(
            RawUser(Long.MaxValue.toString, banned = "invalid", "1234sdf567890", None, None, None)
          ) == Left(InvalidBanned)
        )
      }
    }
  }
}
