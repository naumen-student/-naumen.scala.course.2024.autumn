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
      def empty: RadiusVector =
        new RadiusVector(0, 0)
      def combine(vec1: RadiusVector, vec2: RadiusVector): RadiusVector =
        new RadiusVector(vec1.x + vec2.x, vec1.y + vec2.y)
    }
  }
  case class DegreeAngle(angel: Double)
  object DegreeAngle {
    implicit val monoid: Monoid[DegreeAngle] = new Monoid[DegreeAngle] {
      def empty: DegreeAngle =
        new DegreeAngle(0)
      def combine(deg1: DegreeAngle, deg2: DegreeAngle): DegreeAngle = {
        new DegreeAngle(((deg1.angel % 360 + deg2.angel % 360) % 360))
        // не очень понятные тесты: вроде бы и "GradeAngle всегда выражает угол [0, 360)", а входные углы по модулю
        // получаются очень большими. Плюс не ясно, результирующая сумма двух углов должна быть в данном отрезке
        // неотрицательных значений, или же, например, угол в -150 градусов тоже валиден
        // (по тестам результат может быть отрицателен)
      }
    }
  }

  case class SquareMatrix[A : Monoid](values: ((A, A, A), (A, A, A), (A, A, A)))
  object SquareMatrix {
    implicit def monoid[A: Monoid]: Monoid[SquareMatrix[A]] = new Monoid[SquareMatrix[A]] {
      def empty: SquareMatrix[A] =
        new SquareMatrix[A](
          (Monoid[A].empty, Monoid[A].empty, Monoid[A].empty),
          (Monoid[A].empty, Monoid[A].empty, Monoid[A].empty),
          (Monoid[A].empty, Monoid[A].empty, Monoid[A].empty)
        )
      def combine(matr1: SquareMatrix[A], matr2: SquareMatrix[A]): SquareMatrix[A] = new SquareMatrix[A](
        (
          Monoid[A].combine(matr1.values._1._1, matr2.values._1._1),
          Monoid[A].combine(matr1.values._1._2, matr2.values._1._2),
          Monoid[A].combine(matr1.values._1._3, matr2.values._1._3)
        ),

        (
          Monoid[A].combine(matr1.values._2._1, matr2.values._2._1),
          Monoid[A].combine(matr1.values._2._2, matr2.values._2._2),
          Monoid[A].combine(matr1.values._2._3, matr2.values._2._3)
        ),

        (
          Monoid[A].combine(matr1.values._3._1, matr2.values._3._1),
          Monoid[A].combine(matr1.values._3._2, matr2.values._3._2),
          Monoid[A].combine(matr1.values._3._3, matr2.values._3._3)
        )
      )
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
