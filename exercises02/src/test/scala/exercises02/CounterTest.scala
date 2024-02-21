package exercises02

class CounterTest extends org.scalatest.wordspec.AnyWordSpec {

  "Counter.countWords" should {
    "count" in {
      val text =
        "Привет! Я очень рад тебя видеть!!\nПредставляешь, как я рад. (прим. редактора: Это сарказм)\tГде ты работаешь?\rI'm self-employed.\n I's wonderful"
      val expected = Map(
        "привет"        -> 1,
        "я"             -> 2,
        "очень"         -> 1,
        "рад"           -> 2,
        "тебя"          -> 1,
        "видеть"        -> 1,
        "представляешь" -> 1,
        "как"           -> 1,
        "прим"          -> 1,
        "редактора"     -> 1,
        "это"           -> 1,
        "сарказм"       -> 1,
        "где"           -> 1,
        "ты"            -> 1,
        "работаешь"     -> 1,
        "i's"           -> 1,
        "wonderful"     -> 1,
        "i'm"           -> 1,
        "self-employed" -> 1
      )
      assert(Counter.countWords(text) == expected)
    }

    "one" in {
      val text     = "hello"
      val expected = Map("hello" -> 1)
      assert(Counter.countWords(text) == expected)
    }

    "empty" in {
      val text     = ""
      val expected = Map.empty[String, Int]
      assert(Counter.countWords(text) == expected)
    }
  }

  "Counter.countEnglishWords" should {
    "count" in {
      val text =
        "Привет! Я очень рад тебя видеть!!\nПредставляешь, как я рад. (прим. редактора: Это сарказм)\tГде ты работаешь?\rI'm self-employed.\n I's wonderful"
      val expected = Map("i's" -> 1, "wonderful" -> 1, "i'm" -> 1, "self-employed" -> 1)
      assert(Counter.countEnglishWords(text) == expected)
    }

    "one" in {
      val text     = "hello"
      val expected = Map("hello" -> 1)
      assert(Counter.countEnglishWords(text) == expected)
    }

    "empty" in {
      val text     = ""
      val expected = Map.empty[String, Int]
      assert(Counter.countEnglishWords(text) == expected)
    }
  }

  "Counter.countNumbers" should {
    "count" in {
      val text =
        "Мне еще учится 3 года, как думаешь сколько мне лет?\nЯ думаю 19\rДа ты угадал мне 19"
      val expected = Map("3" -> 1, "19" -> 2)
      assert(Counter.countNumbers(text) == expected)
    }

    "one int" in {
      val text     = "Тут где-то спряталось 1 число"
      val expected = Map("1" -> 1)
      assert(Counter.countNumbers(text) == expected)
    }

    "one float" in {
      val text     = "Дайте мне пожалуйста 1.5 литровую бутылку воды"
      val expected = Map("1.5" -> 1)
      assert(Counter.countNumbers(text) == expected)
    }

    "one russian float" in {
      val text     = "Дайте мне пожалуйста 1,5 литровую бутылку воды"
      val expected = Map("1,5" -> 1)
      assert(Counter.countNumbers(text) == expected)
    }

    "empty" in {
      val text     = ""
      val expected = Map.empty[String, Int]
      assert(Counter.countNumbers(text) == expected)
    }
  }
}
