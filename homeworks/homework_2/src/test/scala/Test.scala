import utest._
import Excercises._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }
        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1, 15) == 60)
            assert(Exercises.sumOfDivBy3Or5(0, 1) == 0)
            assert(Exercises.sumOfDivBy3Or5(-5, 5) == 0)
            assert(Exercises.sumOfDivBy3Or5(-15, -1) == -60)
        }
        'test_primeFactor - {
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(-10) == Seq())
            assert(Exercises.primeFactor(2310) == Seq(2, 3, 5, 7, 11))
            assert(Exercises.primeFactor(2) == Seq(2))
        }
        'test_sumScalars - {
            assert(Exercises.sumScalars(Vector2D(0, 0), Vector2D(0, 0), Vector2D(1, 1), Vector2D(1, 1)) == 2)
            assert(Exercises.sumScalars(Vector2D(-2, -2), Vector2D(2, 2), Vector2D(2, 2), Vector2D(2, 2)) == 0)
            assert(Exercises.sumScalars(Vector2D(5, 5), Vector2D(3, 3), Vector2D(3, 3), Vector2D(5, 5)) == 30 * 2)
            assert(Exercises.sumScalars(Vector2D(3, 3), Vector2D(3, 3), Vector2D(3, 3), Vector2D(3, 3)) == 9 * 4)
        }
        'test_sumCosines - {
            assert(Exercises.sumCosines(Vector2D(1, 0), Vector2D(0, 1), Vector2D(0, 1), Vector2D(1, 0)) == 0)
            assert(Exercises.sumCosines(Vector2D(-2, -2), Vector2D(2, 2), Vector2D(2, 2), Vector2D(2, 2)) == 0)
            assert(Exercises.sumCosines(Vector2D(5, 5), Vector2D(3, 3), Vector2D(3, 3), Vector2D(5, 5)) == 2)
            assert(Exercises.sumCosines(Vector2D(8, 3), Vector2D(3, 3), Vector2D(6, 3), Vector2D(3, 3)) > 1.859)
            assert(Exercises.sumCosines(Vector2D(8, 3), Vector2D(3, 3), Vector2D(6, 3), Vector2D(3, 3)) < 1.86)
        }
        'test_sortByHeavyweight - {
            assert(Exercises.sortByHeavyweight() == Seq(
                "Tin",
                "Platinum",
                "Nickel",
                "Aluminum",
                "Titanium",
                "Lead",
                "Sodium",
                "Uranium",
                "Gold",
                "Tungsten",
                "Zirconium",
                "Chrome",
                "Iron",
                "Copper",
                "Silver",
                "Plutonium",
                "Cobalt",
                "Cesium",
                "Calcium",
                "Lithium",
                "Magnesium",
                "Potassium",
                "Graphite"))
            assert(Exercises.sortByHeavyweight(
                Map(
                    "Name1" -> (1, 1),
                    "Name2" -> (2, 2),
                    "Name3" -> (3, 3)
                )) == Seq("Name1", "Name2", "Name3"))
            assert(Exercises.sortByHeavyweight(
                Map(
                    "D" -> (27, 54),
                    "A" -> (10, 20),
                    "C" -> (20, 40),
                    "B" -> (15, 30),
                )) == Seq("A", "B", "C", "D"))
        }
    }
}
