import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
            assert(Exercises.divBy3Or7(1, 1) == Seq())
            assert(Exercises.divBy3Or7(4, 5) == Seq())
            assert(Exercises.divBy3Or7(7, 5) == Seq())
            assert(Exercises.divBy3Or7(3, 7) == Seq(3, 6, 7))
            assert(Exercises.divBy3Or7(-12, 5) == Seq(-12, -9, -7, -6, -3, 0, 3))
        }

        'sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1, 3) == 3)
            assert(Exercises.sumOfDivBy3Or5(1, 5) == 8)
            assert(Exercises.sumOfDivBy3Or5(5, 9) == 20)
            assert(Exercises.sumOfDivBy3Or5(0, 100) == 2418)
            assert(Exercises.sumOfDivBy3Or5(1, 1) == 0)
            assert(Exercises.sumOfDivBy3Or5(7, 5) == 0)
            assert(Exercises.sumOfDivBy3Or5(-100, 100) == 0)
            assert(Exercises.sumOfDivBy3Or5(-9, -5) == -20)
        }

        'primeFactor - {
            assert(Exercises.primeFactor(5) == Seq(5))
            assert(Exercises.primeFactor(1) == Seq())
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(98) == Seq(2, 7))
            assert(Exercises.primeFactor(100) == Seq(2, 5))
            assert(Exercises.primeFactor(30) == Seq(2, 3, 5))
        }

        'sumScalars - {
            assert(Exercises.sumScalars(Exercises.Vector2D(0, 0), Exercises.Vector2D(0, 0),
                Exercises.Vector2D(0, 0), Exercises.Vector2D(0, 0)) == 0)
            assert(Exercises.sumScalars(Exercises.Vector2D(1, 0), Exercises.Vector2D(0, 0),
                Exercises.Vector2D(0, 0), Exercises.Vector2D(0, 0)) == 0)
            assert(Exercises.sumScalars(Exercises.Vector2D(0, 1), Exercises.Vector2D(0, 0),
                Exercises.Vector2D(0, 0), Exercises.Vector2D(0, 0)) == 0)
            assert(Exercises.sumScalars(Exercises.Vector2D(1, 1), Exercises.Vector2D(0, 0),
                Exercises.Vector2D(0, 0), Exercises.Vector2D(0, 0)) == 0)
            assert(Exercises.sumScalars(Exercises.Vector2D(1, 1), Exercises.Vector2D(1, 1),
                Exercises.Vector2D(1, 1), Exercises.Vector2D(1, 1)) == 4)
            assert(Exercises.sumScalars(Exercises.Vector2D(2, 1), Exercises.Vector2D(1, 2),
                Exercises.Vector2D(2, 3), Exercises.Vector2D(1, 2)) == 4 + 8)
            assert(Exercises.sumScalars(Exercises.Vector2D(5, 3), Exercises.Vector2D(7, 2),
                Exercises.Vector2D(4, 8), Exercises.Vector2D(5, 16)) == 41 + 148)
            assert(Exercises.sumScalars(Exercises.Vector2D(-4, 5), Exercises.Vector2D(-5, 3),
                Exercises.Vector2D(-6, 7), Exercises.Vector2D(-8, -5)) == 35 + 13)

        }

        'sumCosines - {
            assert(Exercises.sumCosines(Exercises.Vector2D(0, 1), Exercises.Vector2D(1, 0),
                Exercises.Vector2D(0, 1), Exercises.Vector2D(1, 0)) == 0)
            assert(Exercises.sumCosines(Exercises.Vector2D(0, 1), Exercises.Vector2D(0, 1),
                Exercises.Vector2D(0, 1), Exercises.Vector2D(0, 1)) == 2)
            assert(Exercises.sumCosines(Exercises.Vector2D(0, 1), Exercises.Vector2D(1, 0),
                Exercises.Vector2D(0, 1), Exercises.Vector2D(0, 1)) == 1)
            assert(Exercises.sumCosines(Exercises.Vector2D(1, 1), Exercises.Vector2D(0, 1),
                Exercises.Vector2D(2, 1), Exercises.Vector2D(2, 3)) == 1.0 / Math.sqrt(2) + 7.0 / 65 * Math.sqrt(65))
        }

        'sortByHeavyweight - {
            assert(Exercises.sortByHeavyweight(Map.empty[String, (Int, Double)]) == Seq())
            assert(Exercises.sortByHeavyweight(Map("Aluminum" -> (3, 2.6889), "Gold" -> (2, 19.32))) == Seq("Aluminum", "Gold"))
            assert(Exercises.sortByHeavyweight(Map("Magnesium" -> (10, 1.738), "Platinum" ->  (1, 21.45), "Cesium" -> (7, 1.873))) == Seq("Platinum", "Cesium", "Magnesium"))
            assert(Exercises.sortByHeavyweight(Map("Tin" -> (1, 7.29))) == Seq("Tin"))
            assert(Exercises.sortByHeavyweight() == Seq("Tin", "Platinum", "Nickel", "Aluminum", "Titanium", "Lead", "Sodium", "Uranium", "Gold", "Tungsten", "Zirconium", "Chrome", "Iron", "Copper", "Silver", "Plutonium", "Cobalt", "Cesium", "Calcium", "Lithium", "Magnesium", "Potassium", "Graphite"))
        }
    }
}
