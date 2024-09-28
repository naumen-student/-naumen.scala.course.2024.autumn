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
            val vec1 = Exercises.Vector2D(1, 0)
            val vec2 = Exercises.Vector2D(0, 1)
            val vec3 = Exercises.Vector2D(1, 1)
            val vec4 = Exercises.Vector2D(-1, -1)

            assert(Exercises.sumScalars(vec1, vec2, vec3, vec4) == 0)

            val vecZero = Exercises.Vector2D(0, 0)
            assert(Exercises.sumScalars(vecZero, vecZero, vec1, vec1) == 0)
        }

        'test_sumCosines - {
            val vec1 = Exercises.Vector2D(1, 0)
            val vec2 = Exercises.Vector2D(0, 1)
            val vec3 = Exercises.Vector2D(1, 1)
            val vec4 = Exercises.Vector2D(-1, -1)

            assert(Exercises.sumCosines(vec1, vec2, vec3, vec4) == 0)

            val vecZero = Exercises.Vector2D(0, 0)
            intercept[ArithmeticException] {
                Exercises.sumCosines(vecZero, vec1, vec1, vecZero)
            }
        }

        'test_sortByHeavyweight - {
            val sortedBalls = Exercises.sortByHeavyweight()
            val expectedResult = Seq(
                "Lithium", "Potassium", "Sodium", "Magnesium", "Calcium", "Cesium", "Graphite", "Silver", 
                "Zirconium", "Chrome", "Tin", "Iron", "Cobalt", "Copper", "Nickel", "Titanium", "Lead", 
                "Uranium", "Plutonium", "Tungsten", "Gold", "Platinum"
            )
            assert(sortedBalls == expectedResult)

            val emptyBalls = Map[String, (Int, Double)]()
            assert(Exercises.sortByHeavyweight(emptyBalls) == Seq())

            val singleBall = Map("TestMetal" -> (5, 10.0))
            assert(Exercises.sortByHeavyweight(singleBall) == Seq("TestMetal"))
        }
    }
}
