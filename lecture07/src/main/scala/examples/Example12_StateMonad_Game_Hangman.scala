package examples

import examples.data.State
import examples.data.State._
import examples.typeclasses.MonadSyntax._

import scala.io.StdIn
import scala.util.Try

/*
Пример игры Виселица с применением State монады
 */
object Example12_StateMonad_Game_Hangman extends App {

  // Для хранения и изменения состояния игры используем State монаду
  type WithState[A] = State[GameState, A]

  class Console {
    def print(str: String): WithState[Unit] = println(str).pure[WithState]
    def getChar: WithState[Char]            = Try(StdIn.readChar()).getOrElse(Char.MinValue).pure[WithState]
  }

  // Класс описывает состояние игры
  final case class GameState(
      guesses: Set[Char],
      word: String
  ) {
    val maxFailures: Int               = (word.toSet.size * 1.5).toInt
    def failures: Int                  = (guesses -- word.toSet).size
    def playerLost: Boolean            = failures >= maxFailures
    def playerWon: Boolean             = (word.toSet -- guesses).isEmpty
    def addChar(char: Char): GameState = copy(guesses = guesses + char)
  }

  sealed trait GuessResult
  object GuessResult {
    case object Won       extends GuessResult
    case object Lost      extends GuessResult
    case object Correct   extends GuessResult
    case object Incorrect extends GuessResult
    case object Unchanged extends GuessResult
  }

  class Hangman(console: Console) {

    private def render(state: GameState): WithState[Unit] = {
      val maskedWord = state.word.map(c => if (state.guesses.contains(c)) c else '*')
      console.print(s"Mistakes: ${state.failures} of ${state.maxFailures}")
      console.print(s"The word: $maskedWord")
    }

    private def analyzeNewInput(oldState: GameState, newState: GameState, char: Char): GuessResult = {
      if (oldState.guesses.contains(char)) GuessResult.Unchanged
      else if (newState.playerWon) GuessResult.Won
      else if (newState.playerLost) GuessResult.Lost
      else if (oldState.word.contains(char)) GuessResult.Correct
      else GuessResult.Incorrect
    }

    private def getCurrentGameState: WithState[GameState]             = State.ask[GameState, GameState](identity)
    private def updateGameState(newState: GameState): WithState[Unit] = State.update[GameState](_ => newState)

    private def gameLoop: WithState[Unit] =
      for {
        _            <- console.print(s"Guess a letter:")
        choice       <- console.getChar.map(_.toLower)
        currentState <- getCurrentGameState
        newState = currentState.addChar(choice)
        _ <- updateGameState(newState)
        _ <- render(newState)
        guessResult = analyzeNewInput(currentState, newState, choice)
        _ <- guessResult match {
          case GuessResult.Won  => console.print(s"You won!")
          case GuessResult.Lost => console.print(s"You lost!")
          case _                => gameLoop
        }
      } yield ()

    def startGame(): WithState[Unit] =
      for {
        _ <- console.print("-- New game! --")
        _ <- gameLoop
      } yield ()
  }

  val hangman: Hangman = new Hangman(new Console)

  // Запуск игра с дефолтным стейтом
  hangman.startGame().run(GameState(Set(), "word"))
}
