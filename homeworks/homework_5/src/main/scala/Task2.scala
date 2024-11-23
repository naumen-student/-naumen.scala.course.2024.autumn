import cats._
import cats.implicits._

/*
  Задание №2
  Всё просто, для каждого кейс класса необходимо описать логику его сложения.
  Радиус-вектор должен складываться, как и любой другой вектор.
  GradeAngle всегда выражает угол [0, 360).
  SquareMatrix просто сложение квадратных матриц
 */
object Task2 extends App {
  case class RadiusVector(x: Int, y: Int)

  object RadiusVector {
    implicit val monoid: Monoid[RadiusVector] = new Monoid[RadiusVector] {
      def empty: RadiusVector = RadiusVector(0, 0)

      def combine(x: RadiusVector, y: RadiusVector): RadiusVector = RadiusVector(x.x + y.x, x.y + y.y)
    }
  }

  case class DegreeAngle(angel: Double)

// тест на сумму неправильный, по условию угол от 0 до 360, в тесте же пытаются сравнивать с отриц значением
  object DegreeAngle {
    implicit val monoid: Monoid[DegreeAngle] = new Monoid[DegreeAngle] {

      override def empty: DegreeAngle = DegreeAngle(0)

      override def combine(x: DegreeAngle, y: DegreeAngle): DegreeAngle = {
        DegreeAngle(normalized(x) + normalized(y))
      }

      def normalized(angle: DegreeAngle): Double = {
        (if (angle.angel < 0) angle.angel % 360 + 360 else angle.angel) % 360
      }
    }
  }

  case class SquareMatrix[A: Monoid](values: ((A, A, A), (A, A, A), (A, A, A)))

  object SquareMatrix {
    implicit def monoid[A: Monoid]: Monoid[SquareMatrix[A]] = new Monoid[SquareMatrix[A]] {
      private def combineRows(row1: (A, A, A), row2: (A, A, A)): (A, A, A) = (
        Monoid[A].combine(row1._1, row2._1),
        Monoid[A].combine(row1._2, row2._2),
        Monoid[A].combine(row1._3, row2._3)
      )

      override def empty: SquareMatrix[A] = {
        val row = (Monoid[A].empty, Monoid[A].empty, Monoid[A].empty)
        SquareMatrix(row, row, row)
      }

      override def combine(a1: SquareMatrix[A], a2: SquareMatrix[A]): SquareMatrix[A] = {
        SquareMatrix((combineRows(a1.values._1, a2.values._1),
          combineRows(a1.values._2, a2.values._2),
          combineRows(a1.values._3, a2.values._3)))
      }
    }
  }

  val radiusVectors = Vector(RadiusVector(0, 0), RadiusVector(0, 1), RadiusVector(-1, 1))
  Monoid[RadiusVector].combineAll(radiusVectors) // RadiusVector(-1, 2)

  val gradeAngles = Vector(DegreeAngle(380), DegreeAngle(60), DegreeAngle(30))
  Monoid[DegreeAngle].combineAll(gradeAngles) // GradeAngle(90)

  val matrixes = Vector(
    SquareMatrix(
      (
        (1, 2, 3),
        (4, 5, 6),
        (7, 8, 9)
      )
    ),
    SquareMatrix(
      (
        (-1, -2, -3),
        (-3, -4, -5),
        (-7, -8, -9)
      )
    )
  )
  Monoid[SquareMatrix[Int]].combineAll(matrixes)
  //  [0, 0, 0]
  //  |1, 1, 1|
  //  [0, 0, 0]
}
