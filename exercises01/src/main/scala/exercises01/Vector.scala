package exercises01

class Vector(val x: Double, val y: Double) {
  def +(other: Vector): Vector = new Vector(other.x + x, other.y + y)

  def -(other: Vector): Vector = new Vector(x - other.x, y - other.y)

  def *(scalar: Double): Vector = new Vector(scalar*x, scalar*y)

  def unary_- : Vector = new Vector(-x, -y)

  def euclideanLength: Double = math.sqrt(x*x + y*y)

  def normalized: Vector = new Vector(x/euclideanLength, y/euclideanLength)

  override def equals(other: Any): Boolean = {
    other match {
      case other_vector: Vector =>
        if (x.equals(other_vector.x) && y.equals(other_vector.y)) {
          return true
        }
      case _ =>
    }
    false
  }

  // Vector(x, y)
  override def toString: String = "Vector(" + x + ", " + y + ")"
}

object Vector {
  def fromAngle(angle: Double, length: Double): Vector = new Vector(math.cos(angle)*length, math.sin(angle)*length)

  def sum(list: List[Vector]): Vector = {
    var vector = new Vector(0, 0)
    for (other <- list) vector += other
    vector
  }

  def unapply(arg: Vector): Option[(Double, Double)] = Some(arg.x, arg.y)
}
