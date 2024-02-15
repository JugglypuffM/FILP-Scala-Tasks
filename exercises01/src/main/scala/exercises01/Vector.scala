package exercises01

class Vector(val x: Double, val y: Double) {
  def +(other: Vector): Vector = ???

  def -(other: Vector): Vector = ???

  def *(scalar: Double): Vector = ???

  def unary_- : Vector = ???

  def euclideanLength: Double = ???

  def normalized: Vector = ???

  override def equals(other: Any): Boolean = ???

  // Vector(x, y)
  override def toString: String = ???
}

object Vector {
  def fromAngle(angle: Double, length: Double): Vector = ???

  def sum(list: List[Vector]): Vector = ???

  def unapply(arg: Vector): Option[(Double, Double)] = ???
}
