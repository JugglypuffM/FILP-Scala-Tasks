package exercises01
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers._
class VectorTest extends org.scalatest.wordspec.AnyWordSpec {
  // sin 45°
  val sin45: Double = Math.sqrt(2) / 2

  // sin 60°
  val sin60: Double = Math.sqrt(3) / 2

  val a = new Vector(10.1, 20.0)
  val b = new Vector(20.3, 23.4)
  val c = new Vector(30.4, -40.156)

  val vectors: List[Vector] = a :: b :: c :: Nil

  val error: Double = 1e-3

  // Asserts that two vectors are equal by comparing their coordinates
  def assertEqualsNeatly(v1: Vector, v2: Vector): Assertion = {
    val xDif = Math.abs(v1.x - v2.x)
    val yDif = Math.abs(v1.y - v2.y)

    if (xDif > error || yDif > error) {
      fail(s"(${v1.x}, ${v1.y}) not equals (${v2.x}, ${v2.y})")
    } else succeed
  }

  "Vector" should {
    "support plus operation" in {
      assertEqualsNeatly(
        a + b, new Vector(30.4, 43.4)
      )
      assertEqualsNeatly(
        a + c, new Vector(40.5, -20.156)
      )
      assertEqualsNeatly(
        b + c, new Vector(50.7, -16.756)
      )
    }

    "support minus operation" in {
      assertEqualsNeatly(
        a - b, new Vector(-10.2, -3.4)
      )
      assertEqualsNeatly(
        a - c, new Vector(-20.3, 60.156)
      )
      assertEqualsNeatly(
        b - c, new Vector(-10.1, 63.556)
      )
    }

    "support multiply on scalar operation" in {
      assertEqualsNeatly(
        a * 12, new Vector(a.x * 12, a.y * 12)
      )
      assertEqualsNeatly(
        b * 15, new Vector(b.x * 15, b.y * 15)
      )
      assertEqualsNeatly(
        c * 0, new Vector(0, 0)
      )
      assertEqualsNeatly(
        a * -2, new Vector(a.x * -2, a.y * -2)
      )
    }

    "distributivity" in {
      assertEqualsNeatly(
        (a + b) * 6, a * 6 + b * 6
      )
      assertEqualsNeatly(
        (b - c) * -2, b * -2 - c * -2
      )
    }

    "support unary minus operation" in {
      vectors.foreach(
        vec => assertEqualsNeatly(-vec, new Vector(-vec.x, -vec.y))
      )
    }

    "support euclideanLength operation" in {
      assert(new Vector(0, 5).euclideanLength == 5)

      assert(new Vector(3, 4).euclideanLength == 5)
      assert(new Vector(-5, 12).euclideanLength == 13)
      assert(new Vector(8, -15).euclideanLength == 17)
      assert(new Vector(-7, -24).euclideanLength == 25)
    }

    "support normalized operation" in new {


      assertEqualsNeatly(
        new Vector(8, -8).normalized,
        new Vector(sin45, -sin45)
      )

      assertEqualsNeatly(
        new Vector(4, 0).normalized,
        new Vector(1, 0)
      )

      assertEqualsNeatly(
        new Vector(0, -2).normalized,
        new Vector(0, -1)
      )
    }

    "support normalized zero operation" in {
      assertEqualsNeatly(
        new Vector(0, 0).normalized,
        new Vector(0, 0)
      )
    }

    "support toString operation" in {
      assert(a.toString == "Vector(10.1, 20.0)")
      assert(b.toString == "Vector(20.3, 23.4)")
    }

    "support fromAngle operation" in {
      assertEqualsNeatly(
        Vector.fromAngle(math.Pi / 6, 10),
        new Vector(sin60 * 10, 5)
      )

      assertEqualsNeatly(
        Vector.fromAngle(math.Pi / 4, 10),
        new Vector(10 * sin45, 10 * sin45)
      )
    }

    "support sum operation" in {
      assertEqualsNeatly(
        Vector.sum(List(a, b, new Vector(10, 10))),
        new Vector(40.4, 53.4)
      )
      assertEqualsNeatly(
        Vector.sum(List(a)),
        a
      )
    }

    "return zero sum on empty list" in {
      assertEqualsNeatly(Vector.sum(List.empty), new Vector(0, 0))
    }

    "support unapply operation" in {
      (a, b, c) match {
        case (Vector(10.1, 20.0), Vector(20.3, 23.4), Vector(30.4, -40.156)) => succeed
        case _ => fail("Метод unapply работает некорректно :(")
      }
    }

    "support equals operation" in {
      vectors.foreach(
        v => assert(v.equals(v))
      )
    }

    "return false on equals with incompatible types" in {
      assertResult(false) (
        a.equals(0)
      )
      assertResult(false)(
        b.equals("Vector(20.3, 23.4)")
      )
      assertResult(false)(
        c.equals((x => x): Nothing => Nothing)
      )
    }
  }
}
