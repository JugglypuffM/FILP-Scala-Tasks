import org.scalatest.wordspec.AnyWordSpec
import state.StatelessRandom.{Random, SimpleRandom}
import state.{BetterStatelessRandom, StatelessRandom}

class StateSpec extends AnyWordSpec {
  "StatelessRandom" should {
    val initial: Random = SimpleRandom(0)

    "pair" in {
      assert(StatelessRandom.pair(initial) == ((5, 71), SimpleRandom(574)))
    }

    "nonNegativeInt" in {
      List.fill(1000)(0).foldLeft(initial) { (acc, _) =>
        val (nextInt, nextState) = StatelessRandom.nonNegativeInt(acc)
        assert(nextInt >= 0)
        nextState
      }
    }

    "double" in {
      List.fill(1000)(0).foldLeft(initial) { (acc, _) =>
        val (nextDouble, nextState) = StatelessRandom.double(acc)
        assert(nextDouble >= 0 && nextDouble <= 1)
        nextState
      }
    }
  }

  "BetterStatelessRandom" should {
    val initial: Random = SimpleRandom(0)
    "nextInt" in {
      assert(BetterStatelessRandom.nextInt.run(initial) == (5, SimpleRandom(41)))
    }

    "pair" in {
      assert(BetterStatelessRandom.pair.run(initial) == ((5, 71), SimpleRandom(574)))
    }

    "nonNegativeInt" in {
      List.fill(1000)(0).foldLeft(initial) { (acc, _) =>
        val (nextInt, nextState) = BetterStatelessRandom.nonNegativeInt.run(acc)
        assert(nextInt >= 0)
        nextState
      }
    }

    "double" in {
      List.fill(1000)(0).foldLeft(initial) { (acc, _) =>
        val (nextDouble, nextState) = BetterStatelessRandom.double.run(acc)
        assert(nextDouble >= 0 && nextDouble <= 1)
        nextState
      }
    }

    "randomList" in {
      val (nextList, _) = BetterStatelessRandom.randomList.run(initial)
      assert(nextList == List(2061449, 158572, 12197, 937, 71))
    }

    "sequence" in {
      val (nextList, _) = BetterStatelessRandom
        .sequence(List(BetterStatelessRandom.nextInt, BetterStatelessRandom.nonNegativeInt))
        .run(initial)
      assert(nextList == List(71, 5))
    }
  }
}
