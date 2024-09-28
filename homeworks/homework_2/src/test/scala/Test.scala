import utest._
import Exercises._

object Test extends TestSuite {

    val tests = Tests {

        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))

            assert(Exercises.divBy3Or7(0, 0) == Seq(0))
            assert(Exercises.divBy3Or7(-10, 0) == Seq(-9, -7, -6, -3, 0))
            assert(Exercises.divBy3Or7(4, 5) == Seq())
        }

        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1, 10) == 33)
            assert(Exercises.sumOfDivBy3Or5(10, 15) == 37)
            assert(Exercises.sumOfDivBy3Or5(0, 100) == 2418)

            assert(Exercises.sumOfDivBy3Or5(0, 0) == 0)
            assert(Exercises.sumOfDivBy3Or5(4, 4) == 0)
            assert(Exercises.sumOfDivBy3Or5(-10, 0) == -33)
            assert(Exercises.sumOfDivBy3Or5(3, 3) == 3)
        }

        'test_primeFactor - {
            assert(Exercises.primeFactor(80) == Seq(2, 5)) // 80 = 2^4 * 5
            assert(Exercises.primeFactor(-80) == Seq(2, 5))
            assert(Exercises.primeFactor(98) == Seq(2, 7)) // 98 = 2 * 7^2
            assert(Exercises.primeFactor(17) == Seq(17)) // простое
            
            assert(Exercises.primeFactor(0) == Seq())
            assert(Exercises.primeFactor(1) == Seq())
            assert(Exercises.primeFactor(2) == Seq(2))
        }

        'test_sumScalars - {
            assert(Exercises.sumScalars(Vector2D(3, 3), Vector2D(3, 3), Vector2D(2, 2), Vector2D(2, 2)) == 26)
            assert(Exercises.sumScalars(Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1)) == 4)
        }

        'test_sumCosines - {
            assert(Exercises.sumCosines(Vector2D(1, 0), Vector2D(0, 1), Vector2D(1, 0), Vector2D(1, 0)) == 1)
            assert(Exercises.sumCosines(Vector2D(1, 0), Vector2D(1, 0), Vector2D(1, 0), Vector2D(1, 0)) == 2)
        }

        'test_sortByHeavyweight - {
            assert(Exercises.sortByHeavyweight(Map("X" -> (3, 2), "Y" -> (2, 20))) == Seq("X", "Y"))

            val defaultResult = Exercises.sortByHeavyweight()
            val expectedResult = Seq(
                "Tin", "Platinum", "Nickel", "Aluminum", "Titanium", "Lead", "Sodium", "Uranium",
                "Gold", "Tungsten", "Zirconium", "Chrome", "Iron", "Copper", "Silver", "Plutonium",
                "Cobalt", "Cesium", "Calcium", "Lithium", "Magnesium", "Potassium", "Graphite"
            )
            assert(defaultResult == expectedResult)

            val emptyBalls = Map[String, (Int, Double)]()
            assert(Exercises.sortByHeavyweight(emptyBalls) == Seq())

            val singleBall = Map("TestMetal" -> (5, 10.0))
            assert(Exercises.sortByHeavyweight(singleBall) == Seq("TestMetal"))
        }
    }
}
