package exercises04.parser

sealed trait Error
object Error {
  case object InvalidBanned   extends Error
  case object Banned          extends Error
  case object InvalidId       extends Error
  case object InvalidName     extends Error
  case object InvalidPassport extends Error
}
