import utest._
import Exercises.Vector2D

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }
        'test_sumDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1, 3) == 3L)
            assert(Exercises.sumOfDivBy3Or5(5, 12) == 42L)
            assert(Exercises.sumOfDivBy3Or5(-12, -1) == -45L)
            assert(Exercises.sumOfDivBy3Or5(-2, 2) == 0L)
        }
        'test_primeFactor - {
            assert(Exercises.primeFactor(3) == Seq(3))
            assert(Exercises.primeFactor(12) == Seq(2, 3))
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(97) == Seq(97))
            assert(Exercises.primeFactor(702) == Seq(2, 3, 13))
        }
        'test_sumScalars - {
            assert(Exercises.sumScalars(Vector2D(-2, 0), Vector2D(2, 0), Vector2D(0, -2), Vector2D(0, 2)) == -8)
            assert(Exercises.sumScalars(Vector2D(3, 0), Vector2D(0, 3), Vector2D(-5, 0), Vector2D(0, -5)) == 0)
            assert(Math.abs(Exercises.sumScalars(Vector2D(2.5, 1.7), Vector2D(-4.6, -8), Vector2D(5.3, 2), Vector2D(0, 7.9)) + 9.3) < 1e-5)
        }
        'test_sumCosines - {
            assert(Exercises.sumCosines(Vector2D(2, 0), Vector2D(0, 2), Vector2D(-2, 0), Vector2D(0, -2)) == 0)
            assert(Math.abs(Exercises.sumCosines(Vector2D(1, 1), Vector2D(1, 0), Vector2D(0, 2), Vector2D(-1, 2)) - 1.60153397) < 1e-5)
        }
        'test_sortByHeavyweight - {
            assert(Exercises.sortByHeavyweight(Exercises.balls)
              == Seq("Tin", "Platinum", "Aluminum", "Sodium", "Nickel", "Titanium", "Lead", "Zirconium", "Chrome",
                "Iron", "Silver", "Uranium", "Lithium", "Gold", "Tungsten", "Copper", "Cesium", "Calcium", "Cobalt",
                "Potassium", "Plutonium", "Magnesium", "Graphite"))
        }
    }
}
