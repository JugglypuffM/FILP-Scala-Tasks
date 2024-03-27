package reader.app

sealed trait Currency
object Currency {
  case object Dollar extends Currency
  case object Euro   extends Currency
  case object Yen    extends Currency
  case object Ruble  extends Currency
}
