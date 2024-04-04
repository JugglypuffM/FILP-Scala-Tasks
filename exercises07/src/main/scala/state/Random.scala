package state

import typeclasses.Monad

object StatefulRandom {

  import scala.util.Random

  // Если вам необходимо сгенерить рандомное число, то вы можете использовать класс scala.util.Random.
  println(Random.nextDouble)
  println(Random.nextInt)
  println(Random.nextInt)

  // Каждый вызов возвращает новое число. Можно предположить, что rnd имеет какое-то внутреннее
  // состояние. Когда происходит очередной вызов nextInt/nextDouble, то rnd не только возвращает
  // новое случайное число, но изменяет внутреннее состояние.
  //
  // Такое скрытое изменение внутреннего состояние имеет ряд недостатков: программы тяжелее
  // композировать, тестировать, усложняется написание многопоточных программ.
}

object StatelessRandom {

  // Чтобы избавиться от скрытого изменяемого состояния, давайте сделаем его явным. Новая функция
  // nextInt возвращает не только случайное число, но и новое состояние генератора случайных чисел.
  trait Random {
    def nextInt: (Int, Random)
  }

  // В качесте простой реализации генератора случайных чисел будем использовать следующий класс
  case class SimpleRandom(seed: Long) extends Random {
    override def nextInt: (Int, Random) = {
      val newSeed = 13 * seed + 41
      val int     = (newSeed >>> 3).toInt
      (int, SimpleRandom(newSeed))
    }
  }

  // и простой пример использования
  val rnd: SimpleRandom     = SimpleRandom(12)
  val (firstInt, nextRnd1)  = rnd.nextInt
  val (secondInt, nextRnd2) = nextRnd1.nextInt
  val (thirdInt, nextRnd3)  = nextRnd2.nextInt
  // Вместо скрытого изменения состояния, мы теперь делаем все изменения явными. Чтобы получать
  // каждый раз разные значения, мы вызываем nextInt на разных объектах: rnd, nextRnd1, nextRnd2 и
  // так далее.

  // Реализуйте функцию, которая будет генерировать пару случайных целых чисел
  def pair(rnd: Random): ((Int, Int), Random) = ((rnd.nextInt._1, rnd.nextInt._2.nextInt._1), rnd.nextInt._2.nextInt._2)

  // Функцию, которая генерирует неотрицальные числа
  def nonNegativeInt(rnd: Random): (Int, Random) = (math.abs(rnd.nextInt._1), rnd.nextInt._2)

  // Функцию, которая генерирует случайное число от нуля включительно до единицы невключительно
  def double(rnd: Random): (Double, Random) = (1 / (rnd.nextInt._1 + 1), rnd.nextInt._2)
}

// Нам удалось избавиться от скрытого изменяемого состояния, но приходится передавать теперь его явно.
// Это приводит к написанию достаточно однообразного кода. Давайте попробуем немного модифицировать
// подход, чтобы избавиться от этого.
object BetterStatelessRandom {

  import typeclasses.Monad.syntax._
  import StatelessRandom.Random

  // Можно заметить, что наши функции pair, nonNegativeInt, double имеют одинаковый шаблон
  // Rnd => (A, Rnd), где тип A зависит от конкретной функции. Можно сказать, что эти функции
  // описывают переход состояния. Давайте обобщим этот шаблон в виде класса RandomState.
  case class RandomState[A](run: Random => (A, Random))

  object RandomState {
    implicit val monad: Monad[RandomState] = new Monad[RandomState] {
      def pure[A](a: A): RandomState[A] = RandomState((a, _))

      def map[A, B](fa: RandomState[A])(f: A => B): RandomState[B] =
        RandomState(rnd => (f(fa.run(rnd)._1), fa.run(rnd)._2))

      def flatMap[A, B](fa: RandomState[A])(f: A => RandomState[B]): RandomState[B] =
        RandomState(rnd => f(fa.run(rnd)._1).run(fa.run(rnd)._2))
    }
  }

  // Теперь класс RandomState может быть использован внутри for comprehension

  // Функция возвращает случайное целое число
  val nextInt: RandomState[Int] = RandomState(_.nextInt)

  // Функция возвращает случайное неотрицальное целое число
  val nonNegativeInt: RandomState[Int] = for {
    v1 <- nextInt
  } yield math.abs(v1)

  // Функция возвращает пару случайных неотрицальных целых чисел
  val pair: RandomState[(Int, Int)] = for {
    v1 <- nextInt
    v2 <- nextInt
  } yield (v1, v2)

  // Функция возвращает случайное число от нуля до единицы
  val double: RandomState[Double] = for {
    v1 <- nextInt
  } yield 1 / (v1 + 1)

  // Функция возвращает список случайной длины из случайных целых чисел
  val randomList: RandomState[List[Int]] = for {
    length <- nextInt
    list   <- sequence(List.fill(length)(nextInt))
  } yield list

  // Функция должна сконвертировать список из случайных состояний в случайное состояние, которое
  // возвращает список.
  def sequence[A](xs: List[RandomState[A]]): RandomState[List[A]] =
    xs.foldLeft(RandomState.monad.pure(List[A]()))((prev, next) => next.flatMap(x => prev.map(b => b.appended(x))))
}
