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
			override def empty: RadiusVector = {
				new RadiusVector(0, 0)
			}

			override def combine(x: RadiusVector, y: RadiusVector): RadiusVector = {
				new RadiusVector(x.x + y.x, x.y + y.y)
			}
		}
	}

	case class DegreeAngle(angle: Double) {
		val angel: Double = ((angle % 360) + 360) % 360
	}

	object DegreeAngle {
		implicit val monoid: Monoid[DegreeAngle] = new Monoid[DegreeAngle] {

			override def empty: DegreeAngle = {
				new DegreeAngle(0);
			}

			override def combine(x: DegreeAngle, y: DegreeAngle): DegreeAngle = {
				new DegreeAngle((x.angel + y.angel) % 360)
			}
		}
	}

	case class SquareMatrix[A: Monoid](values: ((A, A, A), (A, A, A), (A, A, A)))

	object SquareMatrix {
		implicit def monoid[A: Monoid]: Monoid[SquareMatrix[A]] = new Monoid[SquareMatrix[A]] {
			override def empty: SquareMatrix[A] = {
				SquareMatrix(
					(
						(Monoid[A].empty, Monoid[A].empty, Monoid[A].empty),
						(Monoid[A].empty, Monoid[A].empty, Monoid[A].empty),
						(Monoid[A].empty, Monoid[A].empty, Monoid[A].empty)
					)
				)
			}

			override def combine(x: SquareMatrix[A], y: SquareMatrix[A]): SquareMatrix[A] = {
				new SquareMatrix[A](
					combineRow(x.values._1, y.values._1),
					combineRow(x.values._2, y.values._2),
					combineRow(x.values._3, y.values._3)
				)
			}

			private def combineRow(row1: (A, A, A), row2: (A, A, A)): (A, A, A) = {
				(row1._1.combine(row2._1), row1._2.combine(row2._2), row1._3.combine(row2._3))
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
