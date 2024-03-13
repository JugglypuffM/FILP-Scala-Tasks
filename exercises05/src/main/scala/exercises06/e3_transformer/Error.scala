package exercises06.e3_transformer

sealed trait Error
object Error {
  case object InvalidId   extends Error
  case object InvalidName extends Error
}
