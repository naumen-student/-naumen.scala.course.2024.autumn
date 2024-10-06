import Exercises._
import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(divBy3Or7(1, 3) == Seq(3))
            assert(divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }

        'test_sumOfDivBy3Or5 - {
            assert(sumOfDivBy3Or5(1, 10) == 33)
            assert(sumOfDivBy3Or5(1, 15) == 60)
            assert(sumOfDivBy3Or5(1, 0) == 0)
            assert(sumOfDivBy3Or5(-10, -1) == -33)
        }

        'test_primeFactor - {
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(98) == Seq(2, 7))
            assert(Exercises.primeFactor(1) == Seq())
            assert(Exercises.primeFactor(37) == Seq(37))
            assert(Exercises.primeFactor(60) == Seq(2, 3, 5))
        }

        'test_sumScalars - {
            val vec1 = Vector2D(1, 2)
            val vec2 = Vector2D(3, 4)
            assert(Exercises.sumScalars(vec1, vec2, vec1, vec2) == Exercises.scalar(vec1, vec2) + Exercises.scalar(vec1, vec2))
        }

        'test_sumCosines - {
            val vec1 = Vector2D(1, 0)
            val vec2 = Vector2D(0, 1)
            assert(Exercises.sumCosines(vec1, vec2, vec1, vec2) == Exercises.cosBetween(vec1, vec2) + Exercises.cosBetween(vec1, vec2))
        }

        'test_sortByHeavyweight - {
            val expectedOrder = Seq(
                "Tin", "Platinum", "Nickel", "Aluminum", "Titanium", "Lead", "Sodium", "Uranium", "Gold",
                "Tungsten", "Zirconium", "Chrome", "Iron", "Copper", "Silver", "Plutonium", "Cobalt", "Cesium",
                "Calcium", "Lithium", "Magnesium", "Potassium", "Graphite"
            )
            assert(sortByHeavyweight() == expectedOrder)
        }
    }
}
