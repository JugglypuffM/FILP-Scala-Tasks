package exercises06

import exercises06.e3_transformer.Error.{InvalidId, InvalidName}
import exercises06.e3_transformer.{RawUser, TransformerInstances, TransformerSyntax, User, UserName}
import org.scalatest.wordspec.AnyWordSpec

class TransformerSpec extends AnyWordSpec {
  "RawUser" should {
    "have methods transformToOption[User] and transformToEither[User]" in {
      import TransformerInstances._
      import TransformerSyntax._

      assert(
        RawUser("1234", Some("Martin"), Some("Odersky"), None)
          .transformToOption[User]
          .contains(User(1234, UserName("Martin", "Odersky", None)))
      )
      assert(RawUser("abc", Some("Martin"), Some("Odersky"), None).transformToOption[User].isEmpty)
      assert(RawUser("1234", None, Some("Odersky"), None).transformToOption[User].isEmpty)
      assert(RawUser("1234", Some("Martin"), None, None).transformToOption[User].isEmpty)

      assert(
        RawUser("1234", Some("Martin"), Some("Odersky"), None)
          .transformToEither[User]
          .contains(User(1234, UserName("Martin", "Odersky", None)))
      )
      assert(RawUser("abc", Some("Martin"), Some("Odersky"), None).transformToEither[User] == Left(InvalidId))
      assert(RawUser("1234", None, Some("Odersky"), None).transformToEither[User] == Left(InvalidName))
      assert(RawUser("1234", Some("Martin"), None, None).transformToEither[User] == Left(InvalidName))
    }
  }

  "Custom defined Transformer[String, Id]" should {
    "support transformToOption and transformToEither syntax" in {
      import exercises06.e3_transformer.Transformer
      import exercises06.e3_transformer.Error
      type Id = Long

      implicit val transformer: Transformer[String, Id] = new Transformer[String, Id] {
        def toOption(a: String): Option[Id] =
          a.toLongOption

        def toEither(a: String): Either[Error, Id] =
          a.toLongOption.toRight(Error.InvalidId)
      }

      import exercises06.e3_transformer.TransformerSyntax._

      assert("123".transformToOption[Id].contains(123L))
      assert("Kek".transformToOption[Id].isEmpty)
      assert("123".transformToEither[Id] == Right(123L))
      assert("Kek".transformToEither[Id] == Left(InvalidId))
    }
  }
}
