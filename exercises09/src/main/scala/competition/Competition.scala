package competition

import twitter.domain.User

/**
  * Конкурс! Кто наберет больше лайков под своим постом - тот победил
  *
  * Каждый пользовать постит твит "${user.id} will win!", и его фолловеры его лайкают
  * юзеры постят твиты параллельно, и так же параллельно их лайкают фолловеры
  *
  * Но случилась беда: пользователь с именем bot нарушил правила конкурса, и все его лайки надо удалить
  *
  * В конце надо вывести победителя
  * Если победителей несколько, то того, у которого твит был раньше
  * Если победителей нет, то вернуть ошибку TopAuthorNotFound
  *
  * используйте методы
  * CompetitionMethods.unlikeAll
  * CompetitionMethods.topAuthor
  */
trait Competition[F[_]] {
  def winner(
      users: List[User],
      followers: Map[User, List[User]],
      botUser: User
  ): F[User]
}
