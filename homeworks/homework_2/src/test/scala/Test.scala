import Exercises.Vector2D
import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }
        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1, 10) == 33)
            assert(Exercises.sumOfDivBy3Or5(-10, -1) == -33)
            assert(Exercises.sumOfDivBy3Or5(0, 15) == 60)
            assert(Exercises.sumOfDivBy3Or5(-10, 10) == 0)
        }
        'test_primeFactor - {
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(98) == Seq(2, 7))
            assert(Exercises.primeFactor(1) == Seq())
            assert(Exercises.primeFactor(2) == Seq(2))
            assert(Exercises.primeFactor(3) == Seq(3))
            assert(Exercises.primeFactor(100) == Seq(2, 5))
            assert(Exercises.primeFactor(101) == Seq(101))
        }
        'test_sumScalars - {
            val vec1 = Vector2D(1, 2)
            val vec2 = Vector2D(3, 4)
            val vec3 = Vector2D(5, 6)
            val vec4 = Vector2D(7, 8)

            assert(Exercises.sumScalars(vec1, vec2, vec3, vec4) == (Exercises.scalar(vec1, vec2) + Exercises.scalar(vec3, vec4)))
        }

        'test_sumCosines - {
            val vec1 = Vector2D(1, 2)
            val vec2 = Vector2D(3, 4)
            val vec3 = Vector2D(5, 6)
            val vec4 = Vector2D(7, 8)

            assert(Exercises.sumCosines(vec1, vec2, vec3, vec4) == (Exercises.cosBetween(vec1, vec2) + Exercises.cosBetween(vec3, vec4)))
        }
        'test_sortByHeavyweight - {
            val balls3 = Map(
                "Aluminum" -> (3, 2.6889),  "Iron" -> (3, 7.874),
                "Copper" -> (3, 8.96), "Plutonium" -> (3, 19.25),
                "Chrome" -> (3, 7.18), "Zirconium" -> (3, 6.45)
            )
            val expectedOrder3 = Seq("Aluminum", "Zirconium", "Chrome", "Iron", "Copper", "Plutonium")
            assert(Exercises.sortByHeavyweight(balls3) == expectedOrder3)

            val balls2 = Map(
            "Tungsten" -> (2, 19.35), "Gold" -> (2, 19.32), "Nickel" -> (2, 8.91),
                "Lead" -> (2, 11.336), "Titanium" -> (2, 10.50),"Uranium" -> (2, 19.04),
            )
            val expectedOrder2 = Seq("Nickel", "Titanium", "Lead", "Uranium", "Gold", "Tungsten")
            assert(Exercises.sortByHeavyweight(balls2) == expectedOrder2)

            val ballsMix = Map(
                "a" -> (1, 9.0), "b" -> (2, 50.0), "c" -> (120, 1.0), "d" -> (3, 7.0),
            )
            val ballsMixOrder = Seq("a", "d", "b", "c")
            assert(Exercises.sortByHeavyweight(ballsMix) == ballsMixOrder)
        }
    }
}
