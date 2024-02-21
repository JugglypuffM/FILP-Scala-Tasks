package exercises02.game

import exercises02.game.GameController.IGiveUp

import scala.collection.mutable

class GameTest extends org.scalatest.wordspec.AnyWordSpec {
  trait mocks {
    val inQueue: mutable.Queue[String]  = mutable.Queue.empty
    val outQueue: mutable.Queue[String] = mutable.Queue.empty

    val testController: GameController = new GameController {
      override def nextLine(): String = inQueue.dequeue()

      override def askNumber(): Unit = outQueue.enqueue("ask")

      override def numberIsBigger(): Unit = outQueue.enqueue("big")

      override def numberIsSmaller(): Unit = outQueue.enqueue("small")

      override def guessed(): Unit = outQueue.enqueue("guessed")

      override def giveUp(number: Int): Unit = outQueue.enqueue(s"giveUp $number")

      override def wrongInput(): Unit = outQueue.enqueue("wrong")
    }
  }

  "Game" should {
    "ask number on start and end" in new mocks {
      override val inQueue: mutable.Queue[String] = mutable.Queue("2", "3")
      val expectedOut: List[String]               = List("ask", "guessed")

      new Game(testController).play(2)

      assert(outQueue.toList == expectedOut)
      assert(inQueue.toList == List("3"))
    }

    "repeat ask" in new mocks {
      override val inQueue: mutable.Queue[String] = mutable.Queue("1", "3", "2")
      val expectedOut: List[String]               = List("ask", "big", "ask", "small", "ask", "guessed")

      new Game(testController).play(2)

      assert(outQueue.toList == expectedOut)
      assert(inQueue.isEmpty)
    }

    "wrong input" in new mocks {
      override val inQueue: mutable.Queue[String] = mutable.Queue("1", "abc", "2")
      val expectedOut: List[String]               = List("ask", "big", "ask", "wrong", "ask", "guessed")

      new Game(testController).play(2)

      assert(outQueue.toList == expectedOut)
      assert(inQueue.isEmpty)
    }

    "give up" in new mocks {
      override val inQueue: mutable.Queue[String] = mutable.Queue("1", IGiveUp, "2")
      val expectedOut: List[String]               = List("ask", "big", "ask", "giveUp 2")

      new Game(testController).play(2)

      assert(outQueue.toList == expectedOut)
      assert(inQueue.toList == List("2"))
    }

    "real user game" in new mocks {
      override val inQueue: mutable.Queue[String] = mutable.Queue("50", "75", "62", "56", "59", "61", "60a", "60")
      val expectedOut: List[String] =
        List(
          "ask",
          "big",
          "ask",
          "small",
          "ask",
          "small",
          "ask",
          "big",
          "ask",
          "big",
          "ask",
          "small",
          "ask",
          "wrong",
          "ask",
          "guessed"
        )

      new Game(testController).play(60)

      assert(outQueue.toList == expectedOut)
      assert(inQueue.isEmpty)
    }

    "long game" in new mocks {
      override val inQueue: mutable.Queue[String] = mutable.Queue(Range(1, 1000002).map(_.toString): _*)
      val expectedOut: List[String]               = List.fill(1000000)(List("ask", "big")).flatten :+ "ask" :+ "guessed"

      new Game(testController).play(1000001)

      assert(outQueue.toList == expectedOut)
      assert(inQueue.isEmpty)
    }
  }
}
