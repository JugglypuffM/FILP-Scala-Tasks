package exercises01

class ApplicationTest extends org.scalatest.wordspec.AnyWordSpec {

  "Resolver" should {
    "work with 1" in {
      assert(Application.hello("world") == "Hello world")
    }
  }

}
