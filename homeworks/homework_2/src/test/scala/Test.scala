import utest._
import Exercises._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }

        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(0, 4) == 3)
            assert(Exercises.sumOfDivBy3Or5(4, 6) == 5+6)
            assert(Exercises.sumOfDivBy3Or5(0, 6) == 3+5+6)
        }

        'test_primeFactor - {
            assert(Exercises.primeFactor(8) == Seq(2))
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(98) == Seq(2, 7))
        }

        'test_sumScalars - {
            assert(Exercises.sumScalars(Vector2D(1, 0), Vector2D(0, 1), Vector2D(1, 1), Vector2D(0, 0)) == 0.0)
            assert(Exercises.sumScalars(Vector2D(2, 1), Vector2D(1, 1), Vector2D(2, 2), Vector2D(1, 0)) == 5.0)
            assert(Exercises.sumScalars(Vector2D(1, 2), Vector2D(1, 3), Vector2D(1, 4), Vector2D(1, 5)) == 28.0)
        }
        'test_sumCosines - {
            assert(Exercises.sumCosines(Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 0)) == 1.7071067811865475)
            assert(Exercises.sumCosines(Vector2D(-1, 1), Vector2D(1, 1), Vector2D(1, -1), Vector2D(1, -1)) == 0.9999999999999999)
        }
        'test_sortByHeavyweight - {
            assert(Exercises.sortByHeavyweight(
                Map (
                    "Aluminum" -> (12,   2.6889), "Tungsten" ->  (6,   19.35), "Graphite" ->  (1,  2.1),
                    "Magnesium" -> (14,  1.738), "Copper" ->    (123,   8.96)
                )
            ) == Seq("Graphite", "Tungsten", "Aluminum", "Magnesium", "Copper"))
            assert(Exercises.sortByHeavyweight(Map.empty[String, (Int, Double)]) == Seq.empty[String])
        }
    }
}
