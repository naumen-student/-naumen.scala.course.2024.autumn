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
            assert(sumOfDivBy3Or5(1, 10) == 33)
            assert(sumOfDivBy3Or5(-10, 10) == 0)
            assert(sumOfDivBy3Or5(10, 1) == 0)
            assert(sumOfDivBy3Or5(1, 20) == 98)
        }

        'test_primeFactor - {
            assert(primeFactor(80) == Seq(2, 5))
            assert(primeFactor(98) == Seq(2, 7))
            assert(primeFactor(17) == Seq(17))
        }

        'test_sumByFunc - {
            val vec1 = Vector2D(1, 2)
            val vec2 = Vector2D(2, 3)
            val vec3 = Vector2D(3, 4)
            val vec4 = Vector2D(4, 5)

            assert(sumScalars(vec1, vec2, vec3, vec4) == (scalar(vec1, vec2) + scalar(vec3, vec4)))
            assert(sumCosines(vec1, vec2, vec3, vec4) == (cosBetween(vec1, vec2) + cosBetween(vec3, vec4)))
        }

        'test_sortByHeavyweight - {
            val expected = Seq("Tin", "Platinum", "Lead", "Uranium", "Gold", "Tungsten", "Plutonium")
            assert(sortByHeavyweight(Map(
                "Tin" -> (1, 7.29),
                "Platinum" -> (1, 21.45),
                "Tungsten" -> (2, 19.35),
                "Gold" -> (2, 19.32),
                "Plutonium" -> (3, 19.25),
                "Uranium" -> (2, 19.04),
                "Lead" -> (2, 11.336)
            )) == expected)

            assert(Exercises.sortByHeavyweight() == Seq("Tin", "Platinum", "Nickel", "Aluminum", "Titanium", "Lead",
                "Sodium", "Uranium", "Gold", "Tungsten", "Zirconium", "Chrome", "Iron", "Copper", "Silver",
                "Plutonium", "Cobalt", "Cesium", "Calcium", "Lithium", "Magnesium", "Potassium", "Graphite"))

            assert(Exercises.sortByHeavyweight(Map.empty) == Seq())
        }
    }
}
